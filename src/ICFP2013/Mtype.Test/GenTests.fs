namespace Mtype.Tests

open System
open Xunit
open Xunit.Extensions

open Mtype
open Mtype.Gen

module Assert =
    let All(xs : #seq<bool>) =
        Assert.DoesNotContain(false, xs)

    let ForAll<'a>(p : 'a -> bool, xs : seq<'a>) =
        Assert.DoesNotContain(false, Seq.map p xs)

type GenTests () =  
    [<Fact>]
    member x.``minimal`` () =
        let x = List.ofSeq <| emit (1, (set []), (set []))

        Assert.Equal<seq<_>>([BV.Zero; BV.One], x)

    [<Fact>]
    member x.``(not 0)`` () =
        let x = List.ofSeq <| emit (2, (set [Not]), (set ["x"; "y"; "z"]))
        Assert.Contains(BV.Op1 (BV.Not, BV.Zero), x)

    [<Fact>]
    member x.``if + length = 4`` () =
        let x = List.ofSeq <| emit (4, (set [If0]), (set ["x"]))
        Assert.Contains(BV.If0 (BV.Zero, BV.Zero, BV.Zero), x)

    [<Fact>]
    member x.``if, not + length = 4`` () =
        let x = List.ofSeq <| emit (4, (set [If0; Not]), (set ["x"]))
        Assert.Empty(x)

    [<Fact>]
    member x.``if, not + length = 5`` () =
        let x = List.ofSeq <| emit (5, (set [If0; Not]), (set ["x"]))
        Assert.Contains(BV.If0 (BV.Op1 (BV.Not, BV.Zero), BV.Zero, BV.Zero), x)

//    [<Fact>]
//    member x.``of length 1 - 4`` () =
//        let varNames = set ["x"; "y"; "z"; "c"; "v"]
//        let env = Map.ofSeq <| Seq.map (fun x -> x, 666UL) varNames
//        for len in 1 .. 4 do
//            let x = emit (len, allOps, varNames) |> List.ofSeq
//            let rs = Seq.map (fun x -> BV.eval env x) x |> List.ofSeq
//
//            Assert.All(x |> Seq.map (fun x -> BV.lengthOfProgram (BV.Program x) = len + 1))
//
//    [<Fact>]
//    member x.``of length 5 - 6`` () =
//        let varNames = set ["x"]
//        let env = Map.ofSeq <| Seq.map (fun x -> x, 666UL) varNames
//        for len in 5 .. 6 do
//            let x = emit (len, allOps, varNames) |> List.ofSeq
//            let rs = Seq.map (fun x -> BV.eval env x) x |> List.ofSeq
//
//            Assert.All(x |> Seq.map (fun x -> BV.lengthOfProgram (BV.Program x) = len + 1))

    [<Fact>]
    member x.``min program length`` () =
        Assert.Equal(12, minimalProgramLength [If0; And; Xor; Or; Not; Shl1])
        Assert.Equal(2, minimalProgramLength [Not])
        Assert.Equal(3, minimalProgramLength [Not; Shl1])

    [<Fact>]
    member x.``problem TTAl46OnQLl4he76vOs9zu6A`` () =
        let progs = emit (8, set [If0; Plus; Xor], set ["x"]) |> List.ofSeq
        Assert.True(progs.Length > 0)

    [<Fact>]
    member x.``problem cgo4ifsKIKDBJsonkRoaMwyS`` () =
        // (lambda (x) (fold x 0 (lambda y z) (xor y z))))
        let p = BV.Fold (BV.Var "x", BV.Zero, "y", "z", BV.Op2 (BV.Xor, BV.Var "y", BV.Var "z"))
        Assert.Equal(8, BV.lengthOfProgram <| BV.Program p)
        Assert.True((set [TFold; Xor]) = usedOpset p)

        let progs = emit (7, set [TFold; Xor], set ["x"]) |> List.ofSeq
        Assert.True(progs.Length > 0)
        
    [<Fact>]
    member x.``problem YA62PqIfIEU80IsvBykgWBaM`` () =
        // (lambda (x) (fold x 0 (lambda y z) (if0 0 0 0)))
        let progs = emit (8, set [If0; TFold], set ["x"]) |> List.ofSeq
        Assert.True(progs.Length > 0)
    