namespace Mtype.Tests

open System
open Xunit

open Mtype.BV
open Mtype

type EvalTests () =
    [<Fact>]
    member x.``(lambda (x) x) == x`` () =
        Assert.Equal(5UL, evalProgram (Program (Var "x")) 5UL)

    [<Fact>]
    member x.``fold 1`` () =
        let r = evalProgram (Program (Fold (Var "x", Zero, "v_1", "v_2", (Op2 (Or, Var "v_1", Var "v_2"))))) 0x1122334455667788UL
        Assert.Equal(
            0x11UL ||| 0x22UL ||| 0x33UL ||| 0x44UL ||| 0x55UL ||| 0x66UL ||| 0x77UL ||| 0x88UL ||| 0x00UL,
            r)

    [<Fact>]
    member x.``unbound var`` () =
        Assert.Throws<exn>(fun () -> ignore <| evalProgram (Program (Var "y")) 0UL)
        
    [<Fact>]
    member x.``print program 1`` () = 
        let prog1tostr = printProgram (Program(If0 (Op1 (Not,Var "x"),Op2 (Plus,Var "x",One),Zero)))
        let res = "(lambda (x) (if0 (not x) (plus x 1) 0))"
        Assert.Equal<string>(res, prog1tostr)

    [<Fact>]
    member x.``print program 2`` () = 
        let prog2tostr = printProgram (Program(Fold(Var "x", Zero, "y", "z", Op2(Or, Var "y", Var "z"))))
        let res = "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
        Assert.Equal<string>(res, prog2tostr)

    [<Fact>]
    member x.``print program 3`` () = 
        let prog2tostr = printProgramWithVar (Program(Op1(Shr1, Var "x_964"))) (Some "x_964")
        let res = "(lambda (x_964) (shr1 x_964))"
        Assert.Equal<string>(res, prog2tostr)

    [<Fact>]
    member x.``print program 4`` () = 
        let prog2tostr = printProgramWithVar (Program(Op1(Not, Op1(Shr16, Var "x_2782")))) (Some "x_2782")
        let res = "(lambda (x_2782) (not (shr16 x_2782)))"
        Assert.Equal<string>(res, prog2tostr)

    [<Fact>]
    member x.``length of program 1 `` () = 
        let len1 = lengthOfProgram (Program(Fold(Var "x", Zero, "y", "z", Op2(Or, Var "y", Var "z"))))
        Assert.Equal(8, len1)

    [<Fact>]
    member x.``length of program 2 `` () = 
        let len1 = lengthOfProgram (Program(Op1(Shr1, Var "x_2782"))) 
        Assert.Equal(3, len1)

    [<Fact>]
    member x.``length of program 3 `` () = 
        let len1 = lengthOfProgram (Program(Op1(Not, Op1(Shr16, Var "x_2782"))))
        Assert.Equal(4, len1)    


    [<Fact>]
    member x.``compileExpr vc eval`` () =
        let exp1 = 
            BV.Fold (
                BV.Op1 (BV.Shr1,
                    BV.If0 (
                        BV.Op1 (BV.Shr1,
                            BV.Op1 (BV.Shr4, BV.One)),
                        BV.Op2 (BV.And,
                            BV.Var "x",
                            BV.Op1 (BV.Not, BV.Zero)),
                        BV.Var "x")),
                BV.Zero,
                "v_1", "v_2",
                BV.Op2 (BV.Xor,
                    BV.Var "v_1",
                    BV.Op1 (BV.Shl1,
                        BV.Op1 (BV.Shr1,
                            BV.Op1 (BV.Not, BV.Var "v_2")))))

        let exp1Func = compileExpr exp1

        for _ in 1 .. 666 do
            let [input] = Mtype.Game.Numbers.generateRandomValues 1
            Assert.Equal(
                input |> exp1Func,
                input |> (fun x -> eval (x, 0UL, 0UL) exp1))

    [<Fact>]
    member x.``compileExpr speed`` () =
        let exp1 = 
            BV.Fold (
                BV.Op1 (BV.Shr1,
                    BV.If0 (
                        BV.Op1 (BV.Shr1,
                            BV.Op1 (BV.Shr4, BV.One)),
                        BV.Op2 (BV.And,
                            BV.Var "x",
                            BV.Op1 (BV.Not, BV.Zero)),
                        BV.Var "x")),
                BV.Zero,
                "v_1", "v_2",
                BV.Op2 (BV.Xor,
                    BV.Var "v_1",
                    BV.Op1 (BV.Shl1,
                        BV.Op1 (BV.Shr1,
                            BV.Op1 (BV.Not, BV.Var "v_2")))))

        let compiled = BV.compileExpr exp1

        let time f =
            let sw = System.Diagnostics.Stopwatch ()
            sw.Start()
            f ()
            sw.Stop()
            System.Diagnostics.Trace.WriteLine(sprintf "%A" sw.Elapsed)

        let inputs = Mtype.Game.Numbers.generateRandomValues 666666 |> Array.ofList

        time <| fun () -> Array.map compiled inputs |> ignore
        time <| fun () -> Array.map (fun x -> eval (x, 0UL, 0UL) exp1) inputs |> ignore


       