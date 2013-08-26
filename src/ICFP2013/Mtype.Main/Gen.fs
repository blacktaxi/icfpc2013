namespace Mtype

open Utils

module Gen =
    type Op =
        | If0
        | Fold | TFold
        | Not | Shl1 | Shr1 | Shr4 | Shr16
        | And | Or | Xor | Plus
        with
            static member Parse str = 
                match str with
                | "if0" -> If0
                | "fold" -> Fold
                | "tfold"-> TFold
                | "not" -> Not
                | "shl1" -> Shl1
                | "shr1" -> Shr1
                | "shr4" -> Shr4
                | "shr16" -> Shr16
                | "and" -> And
                | "or" -> Or
                | "xor" -> Xor
                | "plus" -> Plus
                | _ -> failwith <| "unknown operator " + str

            static member FromBVOp1 op1 =
                match op1 with
                | BV.Not -> Not
                | BV.Shl1 ->Shl1
                | BV.Shr1 -> Shr1
                | BV.Shr4 -> Shr4
                | BV.Shr16 -> Shr16

            static member FromBVOp2 op2 =
                match op2 with
                | BV.And -> And
                | BV.Or -> Or
                | BV.Xor -> Xor
                | BV.Plus -> Plus
            member x.GetLength() = 
                match x with
                | If0 -> 4
                | Fold | TFold -> 5
                | Not | Shl1 | Shr1 | Shr4 | Shr16 -> 2
                | And | Or | Xor | Plus -> 3

    let isUnary op =
        match op with
        | Not | Shl1 | Shr1 | Shr4 | Shr16 -> true
        | _ -> false

    let isBinary op =
        match op with
        | And | Or | Xor | Plus -> true
        | _ -> false

    let mapUnary op e =
        match op with
        | Not -> BV.Op1 (BV.Not, e)
        | Shl1 -> BV.Op1 (BV.Shl1, e)
        | Shr1 -> BV.Op1 (BV.Shr1, e)
        | Shr4 -> BV.Op1 (BV.Shr4, e)
        | Shr16 -> BV.Op1 (BV.Shr16, e)

    let mapBinary op a1 a2 =
        match op with
        | And -> BV.Op2 (BV.And, a1, a2)
        | Or -> BV.Op2 (BV.Or, a1, a2)
        | Xor -> BV.Op2 (BV.Xor, a1, a2)
        | Plus -> BV.Op2 (BV.Plus, a1, a2)

    let allOps = Set.ofList [If0; Fold; Not; Shl1; Shr1; Shr4; Shr16; And; Or; Xor; Plus]
    
    // set of used operators in expr
    let usedOpset expr = 
        let rec aux expr = 
            match expr with
            | BV.Zero | BV.One | BV.Var(_) -> set<Op>[]
            | BV.If0 (e1,e2,e3) -> Set.unionMany <| seq [set [Op.If0]; aux e1; aux e2; aux e3]
            | BV.Op1 (op1,e1) ->  Set.unionMany <| seq [set [Op.FromBVOp1 op1]; aux e1]
            | BV.Op2 (op2,e1,e2) -> Set.unionMany <| seq [set [Op.FromBVOp2 op2]; aux e1; aux e2]
            | BV.Fold (e1,e2, v1,v2, e3) -> Set.unionMany <| seq [set [Op.Fold]; aux e1; aux e2; aux e3]
        match expr with 
        | BV.Fold (BV.Var(_), BV.Zero, v1,v2, e3) -> Set.unionMany <| seq [set [Op.TFold]; aux e3]
        | _ -> aux expr

    let rec private decompose = memoize decompose'
    and private decompose' (n, x) = [
        if n = 1 then yield [x]
        else
            for y in [1 .. x - n + 1] do
                for d in decompose ((n - 1), (x - y)) do
                    yield y :: d
    ]
    
    let rec private decompose2 x = seq {
        for y in [1 .. x / 2] do
            yield [y; x - y]
    }

    let opLength op =
        match op with
        | Not | Shl1 | Shr1 | Shr4 | Shr16 -> 2
        | And | Or | Xor | Plus -> 3
        | If0 -> 4
        | Fold | TFold -> 5
    
    let rec minimalProgramLength (ops : Op seq) =
        ops 
        |> Seq.map opLength 
        |> Seq.fold (fun r l -> r - 1 + l) 1

    let rec private emit'' = emit'
    and private emit' (len, (ops : Set<Op>), (vars : Set<BV.Name>)) = seq {
            match len with
            | 1 ->
                yield BV.Zero
                yield BV.One
                for v in vars do
                    yield BV.Var v
            | l ->
                // tfold can only be emitted on top level
                let ops' = Set.remove TFold ops
                if l >= 5 then 
                    yield! emitTFold l ops' vars (Set.contains TFold ops)
                    yield! emitFold l ops' vars
                if l >= 4 then yield! emitIf l ops' vars
                if l >= 3 then yield! emitBinary l ops' vars
                if l >= 2 then yield! emitUnary l ops' vars
        }
    and private addNewVar (vars : Set<BV.Name>) =
        let newName = "v_" + vars.Count.ToString()
        Set.add newName vars, newName
    and private emitIf totalLen ops vars = seq {
        // (if1 e1 e2 e3)
        if Set.contains If0 ops then
            for [e1l; e2l; e3l] in decompose (3, (totalLen - 1)) do
                for e1 in emit'' (e1l, ops, vars) do
                    for e2 in emit'' (e2l, ops, vars) do
                        for e3 in emit'' (e3l, ops, vars) do
                            yield BV.If0 (e1, e2, e3)
        }
    and private emitUnary totalLen ops vars = seq {
        // (op1 (op1 e))
        for op1 in Set.filter isUnary ops do
            let e1l = totalLen - 1
            for e1 in emit'' (e1l, ops, vars) do
                yield mapUnary op1 e1
        }
    and private emitBinary totalLen ops vars = seq {
        // (op2 e1 e2)
        for op2 in Set.filter isBinary ops do
            let usedOps' = Set.add op2
            for [e1l; e2l] in decompose2 (totalLen - 1) do
                for e1 in emit'' (e1l, ops, vars) do
                    for e2 in emit'' (e2l, ops, vars) do
                        yield mapBinary op2 e1 e2
        }
    and private emitFold totalLen ops vars = seq {
        // (fold e1 e2 (lambda (var1 var2) e3))
        if Set.contains Fold ops then
            let vars', var1 = addNewVar vars
            let vars'', var2 = addNewVar vars'
            let ops' = Set.remove Fold ops
            for [e1l; e2l; e3l] in decompose (3, (totalLen - 2)) do
                for e1 in emit'' (e1l, ops', vars) do
                    for e2 in emit'' (e2l, ops', vars) do
                        for e3 in emit'' (e3l, ops', vars'') do
                            yield BV.Fold (e1, e2, var1, var2, e3)
        }
    and private emitTFold totalLen ops vars hadTFold = seq {
        // (fold x 0 (lambda (var1 var2) e3))
        if hadTFold then
            let vars', var1 = addNewVar vars
            let vars'', var2 = addNewVar vars'
            let ops' = Set.remove Fold ops

            let e1 = BV.Var "x"
            let e2 = BV.Zero
            let e3l = totalLen - 4
            for e3 in emit'' (e3l, ops', vars'') do
                yield BV.Fold (e1, e2, var1, var2, e3)
        }

    let emit (length, ops, vars) = 
        emit'' (length, ops, vars) |> Seq.filter (fun x -> usedOpset x = ops)

