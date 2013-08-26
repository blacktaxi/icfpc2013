namespace Mtype

open FSharpx

module BV = 
    open Utils

    (*
            program    P ::= "(" "lambda" "(" id ")" e ")"
            expression e ::= "0" | "1" | id
                        | "(" "if0" e e e ")"
                        | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
                        | "(" op1 e ")"
                        | "(" op2 e e ")"
                    op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
                    op2 ::= "and" | "or" | "xor" | "plus" 
                    id  ::= [a-z]+
    *)

    type Program = Program of Expr

    and Expr = 
        | Zero | One | Var of Name
        | If0 of Expr * Expr * Expr
        | Op1 of Op1 * Expr 
        | Op2 of Op2 * Expr * Expr 
        | Fold of Expr * Expr * Name * Name * Expr 
    and Name = string
    and Op1 = 
        | Not | Shl1 | Shr1 | Shr4 | Shr16
    and Op2 = 
        | And | Or | Xor | Plus 

    let rec evalConst = evalConst'
    and private evalConst' expr : uint64 option =
        match expr with
        | Var _ -> None
        | Zero -> Some 0UL | One -> Some 1UL
        | If0 (cond, then', else') ->
            match evalConst cond with
            | Some 0UL -> evalConst then'
            | Some _ -> evalConst else'
            | _ -> 
                match evalConst then', evalConst else' with
                | Some x, Some y when x = y -> Some x
                | _ -> None
        | Op1 (op, expr) ->
            match evalConst expr with
            | Some arg ->
                Some <|
                match op with
                | Not -> ~~~ arg
                | Shl1 -> arg <<< 1
                | Shr1 -> arg >>> 1
                | Shr4 -> arg >>> 4
                | Shr16 -> arg >>> 16
            | _ -> None
        | Op2 (op, expr1, expr2) ->
            match evalConst expr1, op with
            | Some 0xFFFFFFFFFFFFFFFFUL, Or -> Some 0xFFFFFFFFFFFFFFFFUL
            | Some 0UL, And -> Some 0UL
            | Some arg1, _ ->
                match evalConst expr2, op with
                | Some arg2, _ ->
                    Some <|
                    match op with
                    | And -> arg1 &&& arg2
                    | Or -> arg1 ||| arg2
                    | Xor -> arg1 ^^^ arg2
                    | Plus -> arg1 + arg2
                | _ -> None
            | None, Or ->
                match evalConst expr2 with
                | Some 0xFFFFFFFFFFFFFFFFUL -> Some 0xFFFFFFFFFFFFFFFFUL
                | _ -> None
            | None, And ->
                match evalConst expr2 with
                | Some 0UL -> Some 0UL
                | _ -> None
            | _ -> None
        | Fold (inputExpr, initExpr, itemVar, accVar, lambdaExpr) ->
            match evalConst lambdaExpr with
            | Some lambdaConst -> Some lambdaConst
            | _ -> None

    let (|ConstExpr|_|) expr = evalConst expr

    let rec eval (x, v_1, v_2) expr : uint64 =
        match expr with
        | Zero -> 0UL | One -> 1UL
        | Var "x" -> x
        | Var "v_1" -> v_1
        | Var "v_2" -> v_2
//        | If0 (_, then', else') when then' = else' ->
//            eval env then'
        | If0 (cond, then', else') ->
            if (eval (x, v_1, v_2) cond) = 0UL then eval (x, v_1, v_2) then'
            else eval (x, v_1, v_2) else'
        | Op1 (op, expr) ->
            let arg = eval (x, v_1, v_2) expr
            match op with
            | Not -> ~~~ arg
            | Shl1 -> arg <<< 1
            | Shr1 -> arg >>> 1
            | Shr4 -> arg >>> 4
            | Shr16 -> arg >>> 16
        | Op2 (And, Zero, _)
        | Op2 (And, _, Zero) -> 0UL
        | Op2 (And, expr1, expr2) ->
            match eval (x, v_1, v_2) expr1 with
            | 0UL -> 0UL
            | x -> x &&& eval (x, v_1, v_2) expr2
        | Op2 (Or, expr1, expr2) ->
            match eval (x, v_1, v_2) expr1 with
            | 0xFFFFFFFFFFFFFFFFUL -> 0xFFFFFFFFFFFFFFFFUL
            | x -> x ||| eval (x, v_1, v_2) expr2
        | Op2 (op, expr1, expr2) ->
            let arg1, arg2 = eval (x, v_1, v_2) expr1, eval (x, v_1, v_2) expr2
            match op with
            | Xor -> arg1 ^^^ arg2
            | Plus -> arg1 + arg2
        | Fold (_, _, _, _, ConstExpr x) -> x
        | Fold (inputExpr, initExpr, itemVar, accVar, lambdaExpr) ->
            let input = eval (x, v_1, v_2) inputExpr
            let init = eval (x, v_1, v_2) initExpr

            let inputs = 
                [| 0..7 |] |> Array.map 
                    (fun shift -> (input >>> (shift * 8)) &&& 0xffUL)
            inputs |> Array.fold 
                (fun r item -> eval (x, item, r) lambdaExpr)
                init

    let evalProgram prog input =
        match prog with
        | Program e -> eval (input, 0UL, 0UL) e

    let printOp1 op = 
        match op with
        | Not -> "not"
        | Shl1 -> "shl1"
        | Shr1 -> "shr1"
        | Shr4 -> "shr4"
        | Shr16 -> "shr16"

    let printOp2 op = 
        match op with
        | And -> "and"
        | Or -> "or"
        | Xor -> "xor"
        | Plus -> "plus"

    let rec printExpr expr =
        match expr with
        | Zero -> "0"
        | One -> "1"
        | Var(str) -> str
        | If0 (e1,e2,e3) -> sprintf "(if0 %s %s %s)" (printExpr e1) (printExpr e2) (printExpr e3)
        | Op1 (op1,e1) -> sprintf "(%s %s)" (printOp1 op1) (printExpr e1)
        | Op2 (op2,e1,e2) -> sprintf "(%s %s %s)" (printOp2 op2) (printExpr e1) (printExpr e2)
        | Fold (e1,e2, v1,v2, e3) -> sprintf "(fold %s %s (lambda (%s %s) %s))"
                                            (printExpr e1) (printExpr e2) v1 v2 (printExpr e3) 

    let printProgramWithVar program varstr = 

        let x = match varstr with
                | Some(str) -> str
                | None -> "x"
        match program with 
        | Program(expr) -> sprintf "(lambda (%s) %s)" x (printExpr expr)

    let printProgram program = printProgramWithVar program None

    let rec lenOfExpr expr = 
        match expr with
        | Zero | One | Var(_) -> 1
        | If0 (e1,e2,e3) -> 1 + (lenOfExpr e1) + (lenOfExpr e2) + (lenOfExpr e3)
        | Op1 (op1,e1) -> 1 + (lenOfExpr e1)
        | Op2 (op2,e1,e2) -> 1 + (lenOfExpr e1) + (lenOfExpr e2)
        | Fold (e1,e2, v1,v2, e3) -> 2 + (lenOfExpr e1) + (lenOfExpr e2) + (lenOfExpr e3)

    let lengthOfProgram program = 
        match program with 
        | Program(expr) -> lenOfExpr expr |> (+) 1

    let compileExpr expr = 
        let rec compileExpr' expr = 
            match expr with 
            | Zero -> fun (x) -> 0UL
            | One -> fun (x) -> 1UL
            | Var "x" -> fun (x, _, _) -> x
            | Var "v_1" -> fun (_, v1, _) -> v1
            | Var "v_2" -> fun (_, _, v2) -> v2
            | Op1 (op,  e1) ->
                let c1 = compileExpr' e1
                match op with
                | Not -> fun (x) -> ~~~c1(x)
                | Shl1 -> fun (x) -> c1(x) <<< 1
                | Shr1 -> fun (x) -> c1(x) >>> 1
                | Shr4 -> fun (x) -> c1(x) >>> 4
                | Shr16 -> fun (x) -> c1(x) >>> 16
            | Op2 (op2,  e1, e2) -> 
                let c1, c2 = compileExpr' e1, compileExpr' e2
                match op2 with 
                | And -> fun x -> c1(x) &&& c2(x)
                | Or -> fun x -> c1(x) ||| c2(x)
                | Xor -> fun x -> c1(x) ^^^ c2(x)
                | Plus -> fun x -> c1(x) + c2(x)
            | If0 (cond,  e1,  e2) ->
                let cond',  c1, c2 =compileExpr' cond, compileExpr' e1, compileExpr' e2
                fun x -> if cond'(x) = 0UL then c1(x) else c2(x)
            | Fold (inputExpr, initExpr, _,  _, lambdaExpr) ->
                let input = compileExpr' inputExpr 
                let init = compileExpr' initExpr
                let lambda = compileExpr' lambdaExpr

                let funFold (x, y, z) = 
                    let input', init' = input (x, y, z), init(x, y, z)
                    let inputs = 
                        [| 0..7 |] |> Array.map 
                            (fun shift -> (input' >>> (shift * 8)) &&& 0xffUL)
                    inputs |> Array.fold 
                        (fun v2 v1 -> lambda(x,v1,v2))
                        init'
                        
                funFold
        fun x -> (compileExpr' expr)(x, 0UL, 0UL)

//    let rec calculateExpression = memoize calculateExpression'
//    and private calculateExpression' expr =         
//        match expr with 
//        | Zero -> Some(0UL)
//        | One -> Some(1UL)
//        | Var(_) -> None
//        | Op1 (op1, e1) -> 
//            match calculateExpression e1 with
//                | Some(0UL) -> Some(Op1(op1, Zero) |> eval Map.empty)
//                | Some(1UL) -> Some(Op1(op1, One) |> eval Map.empty)
//                | Some(cl_1) -> Some(Op1(op1, Var("cl_1")) |> eval (Map.ofList ["cl_1", cl_1]))
//                | _ -> None
//
//        | Op2 (op2,e1,e2) -> 
//            let e1calc,e2calc = calculateExpression e1,calculateExpression e2
//            match op2 with 
//            | And -> 
//                match e1calc,e2calc with
//                | Some(0UL),_ | _,Some(0UL) -> Some(0UL)
//                | Some(cl_1),Some(cl_2) -> Some(Op2(op2, Var("cl_1"),Var("cl_2")) |> eval (Map.ofList ["cl_1", cl_1; "cl_2", cl_2]))
//                | _ -> None
//            | Or -> 
//                match e1calc,e2calc with
//                | Some(0xFFFFFFFFFFFFFFFFUL),_ | _,Some(0xFFFFFFFFFFFFFFFFUL) -> Some(0xFFFFFFFFFFFFFFFFUL)
//                | Some(cl_1),Some(cl_2) -> Some(Op2(op2, Var("cl_1"),Var("cl_2")) |> eval (Map.ofList ["cl_1", cl_1; "cl_2", cl_2]))
//                | _ -> None                
//            | Plus | Xor -> 
//                match e1calc,e2calc with
//                | Some(cl_1),Some(cl_2) -> Some(Op2(op2, Var("cl_1"),Var("cl_2")) |> eval (Map.ofList ["cl_1", cl_1; "cl_2", cl_2]))
//                | _ -> None
//
//        | If0(e1,e2,e3) ->
//            if (calculateExpression e1 = Some(0UL)) then calculateExpression e2 else calculateExpression e3
//        | Fold(e1,e2,v1,v2,e3) -> calculateExpression e3
//        | _ -> None 

