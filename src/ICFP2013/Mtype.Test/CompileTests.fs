namespace Mtype.Tests

open System
open Xunit

open Mtype.BV
open Mtype
open Compilation

type CompileTests () =
    let compilesTo exp (cexp : CompiledItem list) =
        Assert.Equal<CompiledExpr>(cexp |> Array.ofList, compile exp)

    let fuzzTest' compileFn evalFn exp =
        let compiled = compileFn exp

        for _ in 1 .. 66666 do
            let [input] = Mtype.Game.Numbers.generateRandomValues 1
            Assert.Equal(
                input |> evalFn compiled,
                input |> (fun x -> eval (x, 0UL, 0UL) exp))

    let fuzzTest = fuzzTest' Compilation.compile Compilation.evalCompiled

    [<Fact>]
    member x.``quotation compile 1`` () =
        fuzzTest' Compilation2.compile (fun compiled x -> compiled x) <|
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

    [<Fact>]
    member x.``evalCompiled 1`` () =
        compilesTo (BV.Op1 (BV.Not, BV.Zero)) [CZero; CNot]
        compilesTo (BV.Op2 (BV.And, BV.Zero, BV.One)) [COne; CZero; CAnd]
        compilesTo (BV.If0 (BV.Zero, BV.One, BV.Var "x")) [CZero; CIf0 ([|COne|], [|CVar "x"|])]
        compilesTo 
            (BV.Fold (BV.Zero, BV.One, "v_1", "v_2", BV.Var "x")) 
            [CZero; COne; CFold ([|CVar "x"|])]

        //
        compilesTo
            (BV.Op1 (BV.Not,
                BV.Op1 (BV.Shl1,
                    BV.Op1 (BV.Shr16,
                        BV.Zero))))
            [CZero; CShr16; CShl1; CNot]
        compilesTo
            (BV.Op1 (BV.Not,
                BV.Op2 (BV.And,
                    BV.Op1 (BV.Shr16,
                        BV.Zero),
                    BV.One)))
            [COne; CZero; CShr16; CAnd; CNot]


    [<Fact>]
    member x.``evalCompiled correctness 1`` () =
        fuzzTest <|
            BV.Op1 (BV.Not, BV.Var "x")

    [<Fact>]
    member x.``evalCompiled correctness 2`` () =
        fuzzTest <|
            BV.Op2 (BV.And, BV.Var "x", BV.Zero)

    [<Fact>]
    member x.``evalCompiled correctness 3`` () =
        fuzzTest <|
            (BV.Op1 (BV.Not,
                BV.Op1 (BV.Shl1,
                    BV.Op1 (BV.Shr16,
                        BV.Zero))))

    [<Fact>]
    member x.``evalCompiled correctness 4`` () =
        fuzzTest <|
            (BV.Op1 (BV.Not,
                BV.Op1 (BV.Shl1,
                    BV.Op1 (BV.Shr16,
                        BV.Var "x"))))

    [<Fact>]
    member x.``evalCompiled correctness 5`` () =
        fuzzTest <|
            BV.If0 (BV.One, BV.Zero, BV.Var "x")

    [<Fact>]
    member x.``evalCompiled correctness 6`` () =
        fuzzTest <|
            BV.If0 (BV.Var "x", BV.Zero, BV.One)

    [<Fact>]
    member x.``evalCompiled correctness 7`` () =
        fuzzTest <|
            BV.If0 (BV.Var "x", 
                (BV.Op1 (BV.Not,
                    BV.Op1 (BV.Shl1,
                        BV.Op1 (BV.Shr16,
                            BV.Var "x")))), 
                BV.Op2 (BV.And, BV.Var "x", BV.Zero))

    [<Fact>]
    member x.``evalCompiled correctness 8`` () =
        fuzzTest <|
            BV.Fold (BV.Var "x", BV.Zero, "v_1", "v_2",
                BV.Op2 (BV.Xor, BV.Var "v_1", BV.Var "v_2"))

    [<Fact>]
    member x.``evalCompiled correctness 9`` () =
        fuzzTest <|
            BV.Fold (BV.Var "x", BV.One, "v_1", "v_2", BV.Var "v_1")

    [<Fact>]
    member x.``evalCompiled correctness 10`` () =
        fuzzTest <|
            BV.Fold (BV.Var "x", BV.One, "v_1", "v_2", BV.Var "v_2")

    [<Fact>]
    member x.``evalCompiled correctness 11`` () =
        fuzzTest <|
            BV.Fold (BV.Var "x", BV.One, "v_1", "v_2", BV.Var "x")

    [<Fact>]
    member x.``evalCompiled correctness 12`` () =
        fuzzTest <|
            BV.Fold (BV.One, BV.Var "x", "v_1", "v_2", BV.Var "v_2")

    [<Fact>]
    member x.``evalCompiled correctness 666`` () =
        fuzzTest <|
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

    [<Fact>]
    member x.``evalCompiled speed`` () =
        let exp1 = 
//            BV.If0 (BV.Var "x", 
//                (BV.Op1 (BV.Not,
//                    BV.Op1 (BV.Shl1,
//                        BV.Op1 (BV.Shr16,
//                            BV.Var "x")))), 
//                BV.Op2 (BV.And, BV.Var "x", BV.Zero))
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

        let compiled = Compilation.evalCompiled <| Compilation.compile exp1
        let compiled2 = Compilation2.compile exp1

        let time f =
            let sw = System.Diagnostics.Stopwatch ()
            sw.Start()
            f ()
            sw.Stop()
            System.Diagnostics.Trace.WriteLine(sprintf "%A" sw.Elapsed)

        let inputs = Mtype.Game.Numbers.generateRandomValues 666666 |> Array.ofList

        time <| fun () -> Array.map compiled inputs |> ignore
        time <| fun () -> Array.map compiled2 inputs |> ignore
        time <| fun () -> Array.map (fun x -> eval (x, 0UL, 0UL) exp1) inputs |> ignore
