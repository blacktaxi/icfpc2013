namespace Mtype

open FSharpx
open FSharpx.Collections
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.QuotationEvaluation
open System.Linq

module Compilation2 =
    open BV

    let variable<'TResult> name = 
        let var = Quotations.Var(name, typeof<'TResult>)
//        let expr = Expr.Cast<'TResult>(Quotations.Expr.Var(var))
        let expr = Quotations.Expr.Var(var)
        var, expr

    let compile (expr: BV.Expr) : (uint64 -> uint64) = 
//        let x, derefX = variable<uint64> "x"
        let st, derefSt = variable<uint64 []> "st"
//        let y, derefY = variable<uint64> "y"
//        let z, derefZ = variable<uint64> "z"

        let rec compile' (expr : BV.Expr) =
            match expr with
            | Zero -> <@@ 0UL @@> | One -> <@@ 1UL @@>
            | Var "x" -> <@@ (%%derefSt : uint64 []).[0] @@>
            | Var "v_1" -> <@@ (%%derefSt : uint64 []).[1] @@>
            | Var "v_2" -> <@@ (%%derefSt : uint64 []).[2] @@>
            | Op1 (op, exp) ->
                let sub = compile' exp
                match op with
                | Not -> <@@ ~~~(%%sub : uint64) @@>
                | Shl1 -> <@@ (%%sub : uint64) <<< 1 @@>
                | Shr1 -> <@@ (%%sub : uint64) >>> 1 @@>
                | Shr4 -> <@@ (%%sub : uint64) >>> 4 @@>
                | Shr16 -> <@@ (%%sub : uint64) >>> 16 @@>
            | Op2 (op, expr1, expr2) ->
                let sub1, sub2 = compile' expr1, compile' expr2
                match op with
                | Xor -> <@@ (%%sub1 : uint64) ^^^ (%%sub2 : uint64) @@>
                | Plus -> <@@ (%%sub1 : uint64) + (%%sub2 : uint64) @@>
                | And -> <@@ (%%sub1 : uint64) &&& (%%sub2 : uint64) @@>
                | Or -> <@@ (%%sub1 : uint64) ||| (%%sub2 : uint64) @@>
            | If0 (cond, then', else') ->
                let c, t, e = compile' cond, compile' then', compile' else'
                <@@ if (%%c : uint64) = 0UL then (%%t : uint64) else (%%e : uint64) @@>
            | Fold (inputExpr, initExpr, _, _, lambdaExpr) ->
                let input, init = compile' inputExpr, compile' initExpr
                let lambda = compile' lambdaExpr

                <@@
                    
                    let (input : uint64), (init : uint64) = %%input, %%init

                    (%%derefSt : uint64 []).[2] <- init
                    (%%derefSt : uint64 []).[1] <- (input >>> 0) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 8) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 16) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 24) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 32) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 40) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 48) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)
                    (%%derefSt : uint64 []).[1] <- (input >>> 56) &&& 0xffUL
                    (%%derefSt : uint64 []).[2] <- (%%lambda : uint64)

                    (%%derefSt : uint64 []).[2]

//                    let (input : uint64), (init : uint64) = %%input, %%init
//                    [| 0..7 |] 
//                    |> Array.map (fun shift -> (input >>> (shift * 8)) &&& 0xffUL)
//                    |> Array.fold %%foldFunc init
                @@>

        let inner = compile' expr
            
        let l = Quotations.Expr.Lambda(st, inner)

        let compiled = l.CompileUntyped()() :?> uint64 [] -> uint64
        (fun x -> compiled [|x; 0UL; 0UL|])


module Compilation =
    open BV

    type CompiledExpr = CompiledItem []
    and CompiledItem =
        | CZero | COne | CVar of string
        | CNot | CShl1 | CShr1 | CShr4 | CShr16
        | CAnd | COr | CXor | CPlus
        | CFold of CompiledExpr 
        | CIf0 of CompiledExpr * CompiledExpr

    let rec compile' (expr : BV.Expr) acc =
        match expr with
        | Zero -> CZero :: acc | One -> COne :: acc
        | Var x -> CVar x :: acc
        | Op1 (op, exp) ->
            match op with
            | Not -> compile' exp (CNot :: acc)
            | Shl1 -> compile' exp (CShl1 :: acc)
            | Shr1 -> compile' exp (CShr1 :: acc)
            | Shr4 -> compile' exp (CShr4 :: acc)
            | Shr16 -> compile' exp (CShr16 :: acc)
        | Op2 (op, expr1, expr2) ->
            match op with
            | Xor -> (compile' expr2 (compile' expr1 (CXor :: acc)))
            | Plus -> (compile' expr2 (compile' expr1 (CPlus :: acc)))
            | And -> (compile' expr2 (compile' expr1 (CAnd :: acc)))
            | Or -> (compile' expr2 (compile' expr1 (COr :: acc)))
        | If0 (cond, then', else') ->
            compile' cond (CIf0 (compile then', compile else') :: acc)
        | Fold (inputExpr, initExpr, itemVar, accVar, lambdaExpr) ->
            let input, init = compile' inputExpr [], compile' initExpr []
            compile' inputExpr (compile' initExpr (CFold (compile lambdaExpr) :: acc))
    and compile expr : CompiledExpr = compile' expr [] |> Array.ofList

    let rec evalCompiled' (cexpr : CompiledExpr) x y z : uint64 =
        let stack = System.Collections.Generic.Stack<_>()

        let inline push x = stack.Push(x)
        let inline pop () = stack.Pop()

        for term in cexpr do
            match term with
            | CZero -> push 0UL
            | COne -> push 1UL
            | CVar "x" -> push x
            | CVar "v_1" -> push y
            | CVar "v_2" -> push z
            | CNot -> push (~~~ (pop ()))
            | CShl1 -> push (pop () <<< 1)
            | CShr1 -> push (pop () >>> 1)
            | CShr4 -> push (pop () >>> 4)
            | CShr16 -> push (pop () >>> 16)
            | CAnd -> push ((pop ()) &&& (pop ()))
            | COr -> push ((pop ()) ||| (pop ()))
            | CXor -> push ((pop ()) ^^^ (pop ()))
            | CPlus -> push ((pop ()) + (pop ()))
            | CIf0 (then', else') ->
                let cond = pop ()
                if cond = 0UL then push (evalCompiled' then' x y z)
                else push (evalCompiled' else' x y z)
            | CFold lambda ->
                let init, input = pop (), pop ()

                let r =
                    [| 0..7 |] 
                    |> Array.map (fun shift -> (input >>> (shift * 8)) &&& 0xffUL)
                    |> Array.fold (fun v2 v1 -> evalCompiled' lambda x v1 v2) init
                push r
        pop ()
    let evalCompiled cexpr x = evalCompiled' cexpr x 0UL 0UL

