namespace Mtype

open FSharpx.Option
open FSharpx.Collections
open System.Linq

module NewGen =
    open Gen
    open Utils

    let private log = Log.log "newGEN"

    let rec decomp = decomp'
    and private decomp' (n, x) = [
        if n = 1 then yield [x]
        else
            for y in [0 .. x] do
                for d in decomp ((n - 1), (x - y)) do
                    yield y :: d
    ]

    let rec private takeParts ps xs =
        match ps with
        | [] -> []
        | p :: rest ->
            let x, y = List.splitAt p xs
            x :: (takeParts rest y)
        
    let decompList n xs = seq {
        for ps in decomp (n, List.length xs) do
            yield takeParts ps xs
    }

    let private addNewVar (vars : Set<BV.Name>) =
        let newName = "v_" + vars.Count.ToString()
        Set.add newName vars, newName

    /// emits all programs for given operator occurence order and given var set
    let rec emitForOpOrder = emitForOpOrder'
    and emitForOpOrder' = function
    | ([], inFold) -> seq {
            yield BV.Zero
            yield BV.One
            yield BV.Var "x"
            if inFold then
                yield BV.Var "v_1"
                yield BV.Var "v_2"
        }
    | (op :: rest, inFold) -> seq {
            match op with
            // unary
            | Not | Shl1 | Shr1 | Shr4 | Shr16 ->
                for e1 in emitForOpOrder (rest, inFold) do
                    yield mapUnary op e1
            // binary
            | And | Or | Xor | Plus ->
                for [rest1; rest2] in decompList 2 rest do // @TODO
                    for e1 in emitForOpOrder (rest1, inFold) do
                        let e2s =
                            let all = emitForOpOrder (rest2, inFold)
                            match op, e1 with
                            // (and 0 e) -> 0
                            | And, BV.ConstExpr 0UL -> Seq.firstOrNone all
                            | Or, BV.ConstExpr 0xFFFFFFFFFFFFFFFFUL -> Seq.firstOrNone all
                            | _ -> all
                        for e2 in e2s do
                            yield mapBinary op e1 e2
            | If0 ->
                // (if1 e1 e2 e3)
                for [rest1; rest2; rest3] in decompList 3 rest do
                    for e1 in emitForOpOrder (rest1, inFold) do
                        let e2s, e3s =
                            let e2s = emitForOpOrder (rest2, inFold)
                            let e3s = emitForOpOrder (rest3, inFold)
                            match e1 with
                            // (if0 0 e2 e3) -> e2
                            | BV.ConstExpr 0UL -> e2s, Seq.firstOrNone e3s
                            // (if0 1 e2 e3) -> e3
                            | BV.ConstExpr x when x > 0UL -> Seq.firstOrNone e2s, e3s
                            | _ -> e2s, e3s
                        for e2 in e2s do
                            for e3 in e3s do
                                yield BV.If0 (e1, e2, e3)
            | Fold ->
                // (fold e1 e2 (lambda (var1 var2) e3))
                if inFold then failwith "already inside a fold"
                let inFold' = true
                let var1, var2 = "v_1", "v_2"
                for [rest1; rest2; rest3] in decompList 3 rest do
                    for e3 in emitForOpOrder (rest3, inFold') do
                        let e1s, e2s =
                            let e1s, e2s = emitForOpOrder (rest1, inFold), emitForOpOrder (rest2, inFold)
                            match e3 with
                            | BV.ConstExpr x -> Seq.firstOrNone e1s, Seq.firstOrNone e2s
                            | _ -> e1s, e2s
                        for e1 in e1s do
                            for e2 in e2s do
                                yield BV.Fold (e1, e2, var1, var2, e3)
            | TFold ->
                // (fold x 0 (lambda (var1 var2) e3))
                if inFold then failwith "already inside a fold"
                let inFold' = true
                let var1, var2 = "v_1", "v_2"

                let e1, e2 = BV.Var "x", BV.Zero
                for e3 in emitForOpOrder (rest, inFold') do
                    yield BV.Fold (e1, e2, var1, var2, e3)
        }

    let makeopertators length setofops =
        let rec makeopertators' (setofops : Set<Gen.Op>) setofnotused len = seq {
            match len with
            | 1 when Set.isEmpty setofnotused -> yield []
            | _ when len > 1 -> 
                for x in setofops do
                    let nlen = len - x.GetLength() + 1
                    match nlen with
                    | _ when len <= 0 -> ()
                    | _ -> for l1 in ( nlen  |> makeopertators' 
                                                (if x = Gen.Fold then Set.remove x setofops else setofops)
                                                (Set.remove x setofnotused)) do
                                                    yield x::l1     
            | _ -> ()           
            }

        if (Set.contains Gen.TFold setofops) then 
            let newset = Set.remove Gen.TFold setofops
            seq {
                for tl in(length - Gen.TFold.GetLength() + 1 |> makeopertators' newset newset) do
                    yield Gen.TFold::tl
            }
        else 
            makeopertators' setofops setofops length

    let approxOporders len opset =
        let apx = 
            opset
            |> Seq.fold (fun r (op : Gen.Op) ->
                r * (len / (op.GetLength() - 1)))
                1
        uint64 <| (double apx) ** (1.0 / 2.1)

    let emitForLengthAndOpset (len, opset) =
        log.Debug "Emitting exprs: %d, %A" len opset
        let opOrders = makeopertators len opset |> Array.ofSeq
        log.Debug "Processing %d oporders..." opOrders.Length

        let r = opOrders.AsParallel().SelectMany(fun ops -> emitForOpOrder (ops, false)) |> Array.ofSeq
        log.Debug "Done emitting, total %d results." r.Length
        r

    let emitForLengthAndOpsetLimited (len, opset) (maxOpOrders : int) =
        log.Debug "Emitting exprs: %d, %A" len opset
        log.Debug "Generating oporders"

        let approxOrders = approxOporders len opset
        log.Debug "Approximate %d oporders" approxOrders
        if approxOrders > (uint64 maxOpOrders) * 2UL then
            log.Error "Too many APPROX oporders."
            None
        else
            maybe {
                let! opOrders =
                    try
                        let comp = async { return makeopertators len opset |> Array.ofSeq }
                        Some <| Async.RunSynchronously(comp, 5000)
                    with exn ->
                        log.Warn "Failed to generate oporders: %A" exn
                        None

                if opOrders.Length > maxOpOrders then
                    log.Error "Too many oporders! (%d vs %d)" opOrders.Length maxOpOrders
                    ()
                else
                    log.Debug "Processing %d oporders..." opOrders.Length
                    let r = opOrders.AsParallel().SelectMany(fun ops -> emitForOpOrder (ops, false)) |> Array.ofSeq
                    log.Debug "Done emitting, total %d results." r.Length
                    return r
            }

//        |> Seq.map (fun ops -> async { return emitForOpOrder (ops, vars) })
//        |> Async.Parallel
//        |> Async.RunSynchronously
//        |> Seq.concat
//        |> Seq.ofArray
