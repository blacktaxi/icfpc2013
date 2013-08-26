namespace Mtype

open Utils


module Deriv =
    
    let makeUnary (setofoper:Set<Gen.Op>) init n = seq {
        match n with
        | 1 -> 
        for x in setofoper do
            if x.GetLength() = 2 then 
                for expr, l in init do
                    if l = n-1 then
                        yield Gen.mapUnary x expr, l+1
        }

    let makeBinary (setofoper:Set<Gen.Op>) init n = seq {
        for x in setofoper do
            if x.GetLength() = 3 then 
                for e1, l1 in init do
                    if l1 = n-1 then
                        for e2,l2 in init do
                            yield Gen.mapBinary x e1 e2, l1+l2+1
        }


    let rec genfunctionlessthan (setofoper:Set<Gen.Op>) init n = seq {
        match n with 
        | 0 -> yield! init
        | _ -> 
            let prev = n - 1 |> genfunctionlessthan setofoper init
            yield! prev
            yield! makeUnary setofoper prev n
            yield! makeBinary setofoper prev n
        }

    let run n = 
        let init = [BV.Zero,1; BV.One,1; BV.Var "x", 1]
        genfunctionlessthan (set[Gen.Or; Gen.Not; Gen.Shl1]) init n
        
