namespace Mtype.Tests

open System
open Xunit
open Xunit.Extensions

open Mtype
open Mtype.Gen
open Mtype.NewGen

type NewGenTests () =  
    [<Fact>]
    member x.``minimal`` () =
        let ps = emitForOpOrder ([], false)
        Assert.Contains(BV.Zero, ps)

    [<Fact>]
    member x.``single`` () =
        let ps = emitForOpOrder ([Not], false)
        Assert.Contains(BV.Op1 (BV.Not, BV.Zero), ps)

    [<Fact>]
    member x.``two unary`` () =
        let ps = emitForOpOrder ([Not; Shl1], false)
        Assert.Contains(BV.Op1 (BV.Not, BV.Op1 (BV.Shl1, BV.Zero)), ps)

    [<Fact>]
    member x.``one binary`` () =
        let ps = emitForOpOrder ([Or], false)
        Assert.Contains(BV.Op2 (BV.Or, BV.One, BV.Zero), ps)

    [<Fact>]
    member x.``one binary, one unary`` () =
        let ps = emitForOpOrder ([Or; Not], false)
        Assert.Contains(BV.Op2 (BV.Or, BV.One, BV.Op1 (BV.Not, BV.Zero)), ps)

    [<Fact>]
    member x.``two binary`` () =
        let ps = emitForOpOrder ([And; Or], false)
        Assert.Contains(
            BV.Op2 (BV.And, 
                BV.One,
                BV.Op2 (BV.Or, 
                    BV.Var "x",
                    BV.Zero)), 
            ps)

    [<Fact>]
    member x.``two binary, one unary`` () =
        let ps = emitForOpOrder ([And; Or; Not], false) |> List.ofSeq
        Assert.Contains(
            BV.Op2 (BV.And, 
                BV.Op2 (BV.Or,
                    BV.Zero,
                    BV.Var "x"),
                BV.Op1 (BV.Not, BV.Zero)), 
            ps)

    [<Fact>]
    member x.``one if`` () =
        let ps = emitForOpOrder ([If0], false) |> List.ofSeq
        Assert.Contains(
            BV.If0 (
                BV.Var "x",
                BV.Zero,
                BV.One),
            ps)

    [<Fact>]
    member x.``many`` () =
        let ps = emitForOpOrder ([If0; Plus; Or; Not], false) |> List.ofSeq
        let ps' = emitForLengthAndOpset (9, (set [If0; Plus; Or; Not]))
        let p =
            BV.If0 (
                BV.Op2 (BV.Plus,
                    BV.Var "x",
                    BV.Op2 (BV.Or,
                        BV.Zero,
                        BV.Op1 (BV.Not,
                            BV.Var "x"))),
                BV.Var "x",
                BV.Zero)

        Assert.Contains(p, ps)
        Assert.Contains(p, ps')

    [<Fact>]
    member x.``super many`` () =
        let ps = emitForOpOrder ([Shr1; If0; Shr1; Shr4; And; Not], false) |> List.ofSeq
        Assert.Contains(
            BV.Op1 (BV.Shr1,
                BV.If0 (
                    BV.Op1 (BV.Shr1,
                        BV.Op1 (BV.Shr4, BV.One)),
                    BV.Op2 (BV.And,
                        BV.Var "x",
                        BV.Op1 (BV.Not, BV.Zero)),
                    BV.Var "x")),
            ps)

    [<Fact>]
    member x.``big fold`` () =
        let ps = emitForOpOrder ([Fold; Not; Shr1; Plus; Shr1], false) |> List.ofSeq
        Assert.Contains(
            BV.Fold (
                BV.Op1 (BV.Not,
                    BV.Op1 (BV.Shr1, BV.Var "x")),
                BV.Var "x",
                "v_1", "v_2",
                BV.Op2 (BV.Plus,
                    BV.Var "v_1",
                    BV.Op1 (BV.Shr1, BV.Var "v_2"))),
            ps)

    [<Fact>]
    member x.``tfold`` () =
        let ps = emitForOpOrder ([TFold; Xor; Shl1; Shr1; Not], false) |> List.ofSeq
        Assert.Contains(
            BV.Fold (
                BV.Var "x", BV.Zero,
                "v_1", "v_2",
                BV.Op2 (BV.Xor,
                    BV.Var "v_1",
                    BV.Op1 (BV.Shl1,
                        BV.Op1 (BV.Shr1,
                            BV.Op1 (BV.Not, BV.Var "v_2"))))),
            ps)

    [<Fact>]
    member x.``makeopertators contains list `` () =        
        Assert.Contains([Op.And; Op.And; Op.Or; Op.Or; Op.And], 
                set[Op.Or;Op.And] |> makeopertators 11 )

    [<Fact>]
    member x.``makeopertators doesn't contains list `` () =        
        Assert.DoesNotContain([Op.And; Op.And; Op.And; Op.And; Op.And], 
                set[Op.Or;Op.And] |> makeopertators 11 )


    [<Fact>]
    member x.``makeopertators contains one fold`` () =        
        Assert.ForAll((fun y-> y |> List.filter(fun z -> z=Op.Fold) |> List.length = 1), 
                set[Op.Or;Op.Fold] |> makeopertators 11 )

    [<Fact>]
    member x.``makeopertators head = tfold`` () =        
        Assert.ForAll((fun y-> (List.head y) = Op.TFold), 
                set[Op.Or;Op.TFold] |> makeopertators 11 )


    [<Fact>]
    member x.``generated length is correct`` () =
        let ps = emitForLengthAndOpset (7, (set [Plus; Shr4]))

        Assert.Contains(
            BV.Op2 (BV.Plus,
                BV.Var "x",
                BV.Op2 (BV.Plus,
                    BV.Op1 (BV.Shr4, BV.Var "x"),
                    BV.Op1 (BV.Shr4, BV.Zero))),
            ps)

        Assert.ForAll((fun x -> BV.lenOfExpr x = 7), ps)

//    [<Fact>]
//    member x.``calculateExpression (Not ( And(1, Not 1)))`` () =         
//        Assert.Equal(
//            Some(~~~(1UL &&& (~~~1UL))),
//            (BV.Op1(BV.Not, BV.Op2(BV.And,BV.One, BV.Op1(BV.Not, BV.One))) |> BV.calculateExpression )
//            )
