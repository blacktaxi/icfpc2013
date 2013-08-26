namespace Mtype

open System
open System.Linq

module Utils =
    let memoize f = 
        let cache = System.Collections.Generic.Dictionary<_,_>(HashIdentity.Structural)
        fun x ->
            let ok, res = cache.TryGetValue(x)
            if ok then res
            else 
                let res = f x
                cache.[x] <- res
                res

    let go = Async.RunSynchronously

module List =
    let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
  
    let rec permutations = function
    | []      -> seq [[]]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

module Seq =
    let firstOrNone xs = Enumerable.Take(xs, 1)