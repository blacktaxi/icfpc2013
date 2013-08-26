namespace Mtype

open System
open System.Linq

module Game = 
    let private log = Log.log "player"

    module Numbers =     
        let generateRandomValues n = 
            let r = Random()
            let bs = Array.zeroCreate 8 : byte[]
            seq { 
                while true do
                    r.NextBytes(bs)
                    yield BitConverter.ToUInt64(bs,0)
                } |> Seq.take n |> Seq.toList

        let toUInt64HexString (x:uint64) = 
            sprintf "0x%X" x

        let toUInt64 (str:string) = 
            let s1 = if str.StartsWith("0x") then str.Substring(2) else str
            match UInt64.TryParse(s1, Globalization.NumberStyles.HexNumber,null) with
            | true, value -> value
            | false,_ -> failwith <| "can't parse string to UInt64, str = " + str

        let getRandomarguments256 () = 
            generateRandomValues 256 |> List.map toUInt64HexString

    module Requests = 
        open Numbers
        open Utils
        open Api
        open Data

        type CompiledExpr (expr : BV.Expr) =
            let mutable compiled = None

            member x.Compile() =
                match compiled with
                | None -> compiled <- Some <| Compilation.compile expr
                | _ -> ()

            member x.Eval(xx) =
                match compiled with
                | Some c -> Compilation.evalCompiled c xx
                | None -> BV.eval (xx, 0UL, 0UL) expr

            member x.Expr with get () = expr

        type MakeEvalRequest = 
            | MOk of seq<uint64*uint64>
            | MGone 
            | MAlreadySolved
            | MOther

        let private getRandom exprseq = 
            let l =  Seq.length exprseq
            let r = System.Random().Next(l)
            exprseq |> Seq.skip r |> Seq.head

        let rec makeEvalRequest problemId =
            log.Debug "Eval'ing for problem %s" problemId
            let arguments = getRandomarguments256 ()
            let res = evalById problemId arguments |> Async.RunSynchronously
            log.Debug "Processing response..."
            match res with
            | Ok(v) -> MOk(Seq.map2 ( fun x y -> (toUInt64 x,toUInt64 y) ) arguments v.outputs)
            | TryAgainLater -> 
                log.Error "TryAgainLater: wait 1 second"
                System.Threading.Thread.Sleep(1000)
                makeEvalRequest problemId
            | BadRequest | Unauthorized | AuthRequired | NotFound | RequestTooBig | Fail(_,_) -> 
                log.Fatal "Exception %A" res
                MOther
            | Gone -> 
                log.Fatal "problem requested more than 5 minutes ago"
                MGone
            | AlreadySolved -> 
                log.Warn "problem was already solved (by current user)"
                MAlreadySolved
        
        let filterExpressions argVals (expressions : CompiledExpr seq) =
            expressions.AsParallel().Where(fun (cexpr : CompiledExpr) ->
                argVals
                |> Seq.forall (fun (arg1 : uint64, val1) -> 
                    val1 = cexpr.Eval(arg1)))

        let precompileExprs (cexprs : CompiledExpr seq) =
            log.Debug("Compiling exprs...")
            for e in cexprs do e.Compile()
            log.Debug("Done!")

        let rec guessProgram problemId expressions program =
            log.Info "Guessing problem %s with '%s'" problemId program
            match program |> Api.guess problemId |> Async.RunSynchronously with
            | Ok r -> 
                match r with
                | Win -> 
                    log.Info "<<<<<<< WIN! >>>>>>>>"
                    true
                | Error msg -> failwith <| sprintf "Program is incorrect: %s, %A" msg program
                | Mismatch (input, theirs, ours) ->
                    log.Warn "Mismatch: p(%s) should be %s, not %s" input theirs ours
                    log.Debug "Filtering by mismatch..."
                    let exs = 
                        expressions
                        |> filterExpressions (Seq.singleton(toUInt64 input, toUInt64 theirs))
                        |> Array.ofSeq
                    log.Debug "Done."
                    guessLoop problemId exs false true
            | Gone ->
                log.Fatal "Time's up for problem %s" problemId
                false
            | AlreadySolved ->
                log.Warn "Already solved... :|"
                true 
            | TryAgainLater -> 
                log.Error "TryAgainLater: wait 1 second"
                System.Threading.Thread.Sleep(1000)
                guessProgram problemId expressions program
            | _ -> false
        
        and guessLoop problemId (expressions : CompiledExpr []) firstIter compiledAlready =
            match makeEvalRequest problemId with
            | MOk(argVals) ->
                let compiledThisTime =
                    if firstIter then
                        log.Debug "First iteration -- not compiling programs this time."
                        false
                    else
                        if compiledAlready then
                            log.Debug "Programs are compiled already -- skipping."
                            false
                        else
                            precompileExprs expressions
                            false
                let lenofexprOld = expressions.Length
                log.Debug "Filtering programs..."
                let expressions = (filterExpressions argVals expressions).ToArray()
                let lenofexprNew = expressions.Length
                log.Debug "Done filtering, %d programs left." lenofexprNew
                match Seq.length expressions with 
                | 0 ->
                    log.Fatal "ALL programs were filtered out -- can't continue."
                    failwith "expressions count = 0"
                | 1 -> expressions.First().Expr |> BV.Program |> BV.printProgram |> guessProgram problemId expressions
                | _ when lenofexprNew < lenofexprOld ->
                    log.Debug "Filtered out %d invalid programs!" (lenofexprOld - lenofexprNew)
                    guessLoop problemId expressions false (compiledAlready || compiledThisTime)
                | _  -> (getRandom expressions).Expr |> BV.Program |> BV.printProgram |> guessProgram problemId expressions
            | MOther -> failwith "error"
            | MGone -> false
            | MAlreadySolved -> true

        let isProgramSolved problem =
            if problem.solved then true
            else
                try
                    let p = GenCache.loadProblem problem.id
                    p.solved
                with exn -> false
//                match Api.myproblems () |> Async.RunSynchronously with
//                | Ok seq1 ->
//                    (seq1 |> Seq.find (fun x-> x.id = problem.id)).solved
//                | _ -> false

        let parseOperators (ops : string seq) =
            ops
            |> Seq.filter (fun x -> x <> "bonus")
            |> Seq.map Gen.Op.Parse
        
        let solveProblem (problem : Problem) (maxOpOrders : int option) =
            log.Info "Solving %A..." problem
            let operators = problem.operators |> parseOperators |> set
            log.Debug "Generating exprs for size %d and ops %A..." (problem.size - 1) operators

            let expressions =
                match GenCache.loadPrograms problem.id with
                | Some exprs -> 
                    log.Debug "Loaded programs from cache."
                    Some exprs
                | None ->
                    match maxOpOrders with
                    | Some maxorders ->
                        log.Warn "NO cached programs found. Generating, limited by %d oporders..." maxorders
                        NewGen.emitForLengthAndOpsetLimited (problem.size - 1, operators) maxorders
                    | None -> 
                        log.Warn "NO cached programs found. Generating..."
                        Some <| NewGen.emitForLengthAndOpset (problem.size - 1, operators)

            match expressions with
            | Some expressions ->
                log.Info "Generated/loaded %d programs." expressions.Length
                if expressions.Length = 0 then
                    log.Error "NO programs generated for problem."
                    false
                else
                    let compExprs = Array.map (fun e -> CompiledExpr(e)) expressions

                    let sw = System.Diagnostics.Stopwatch()
                    sw.Start()

                    log.Info "Starting guessloop..."
                    let r = guessLoop problem.id compExprs true true

                    sw.Stop()
                    log.Info "Solved %s in %A!" problem.id sw.Elapsed
                    r
            | None ->
                log.Warn "No programs generated -- see logs. Problem not solved."
                false
