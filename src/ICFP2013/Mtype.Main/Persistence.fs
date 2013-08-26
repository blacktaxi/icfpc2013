namespace Mtype

open System
open FsCoreSerializer

module Persistence =
    let private fsc = new FsCoreSerializer() :> ISerializer

    let serialize x = fsc.Serialize x
    let deserialize<'a> (x : byte []) = (fsc.Deserialize x) :?> 'a

    let saveToFile x fname =
        System.IO.File.WriteAllBytes(fname, serialize x)

    let loadFromFile<'a> fname =
        System.IO.File.ReadAllBytes(fname) |> deserialize<'a>
