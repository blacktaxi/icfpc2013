namespace Mtype.Tests

open System
open Xunit

open Mtype.Persistence

type PersistenceTests () =
    let data = Mtype.Gen.emit (4, Mtype.Gen.allOps, (set ["x"])) |> Array.ofSeq

    [<Fact>]
    member x.``basic save/load`` () =
        let f = System.IO.Path.GetTempFileName()

        saveToFile data f
        let data' = loadFromFile<Mtype.BV.Expr []> f

        Assert.Equal<Mtype.BV.Expr seq>(data, data')
