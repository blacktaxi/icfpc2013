namespace Mtype

open System
open Mtype.Game.Numbers

open Mtype.Data

module CLI =
    let private log = Log.log "playerCLI"

    [<EntryPoint>]
    let main argv = 
        match List.ofArray argv with
        | ["status"] ->
            let (Api.Ok stat) = Api.status () |> Async.RunSynchronously
            log.Info "Current team status:\n%A" stat

        | ["probstats"] ->
            GenCache.getProblems ()
            let probs = 
                GenCache.loadProblems () |> List.sortBy (fun x -> 
                    (x.solved, x.size, not <| Seq.exists (fun x -> x = "tfold") x.operators, Seq.length <| x.operators))
            for p in probs do
                log.Info "%s: /%d/ [%s] %s" p.id p.size (String.Join(", ", p.operators))
                    (if p.solved then "*SOLVED*" else "not solved")

        | ["shit"] ->
            let probs = 
                GenCache.loadProblems () 
                |> List.sortBy (fun x -> (x.solved, x.size))
                |> List.filter (fun x -> x.size = 10)
            for p in probs do
                let ops = Seq.map Gen.Op.Parse p.operators |> Set.ofSeq
                log.Debug "%d / %d" (NewGen.approxOporders p.size ops) (Seq.length (NewGen.makeopertators (p.size - 1) ops))

        | "solvetrain" :: len :: ops ->
            let len = Int32.Parse len
            log.Info "Getting train problem of length %d with operators: %A..." len ops
            let (Api.Ok tprob) = Api.train len ops |> Async.RunSynchronously
            log.Info "Train problem: %A" tprob
            log.Info "Solving train problem"
            Game.Requests.solveProblem 
                { Problem.id = tprob.id; size = tprob.size; operators = tprob.operators; solved = false; timeLeft = 666.0 }
                None
            |> ignore

        | "solvetrainhack" :: len :: genLen :: ops ->
            let len = Int32.Parse len
            let genLen = Int32.Parse genLen
            log.Info "Getting train problem of length %d with operators: %A..." len ops
            let (Api.Ok tprob) = Api.train len ops |> Async.RunSynchronously
            log.Info "Train problem: %A" tprob
            log.Info "Solving train problem"
            Game.Requests.solveProblem 
                { Problem.id = tprob.id; size = genLen; operators = tprob.operators; solved = false; timeLeft = 666.0 }
                None
            |> ignore

        | "romanianmode" :: maxLevel :: [] ->
            let level = Int32.Parse maxLevel
            let problems = Mtype.GenCache.loadProblems ()

            let estimateLevel (problem : Problem) =
                let ops = Set.remove "bonus" <| Set.ofSeq problem.operators
                if Set.contains "tfold" ops then
                    (Seq.length ops) * 2 - 4
                else
                    (Seq.length ops) * 2

            let rnd = System.Random()
            for p in problems |> Seq.filter (fun p -> estimateLevel p <= level && (not p.solved)) |> Seq.sortBy (fun _ -> rnd.Next()) do
                let l = estimateLevel p
                log.Info "Problem %s is estimated to be of level %d" p.id l
                log.Debug "%A" p

                let haveTFold = Set.contains "tfold" <| Set.ofSeq p.operators
                let mutable curGenLen = 11
                let maxGenLen = if haveTFold then 15 else 13
                let mutable continueTrying = true
                while (curGenLen <= maxGenLen && continueTrying) do
                    log.Info "Attempting to solve with gensize = %d" curGenLen
                    try
                        let solved = Game.Requests.solveProblem { p with size = curGenLen } (Some 1080)
                        if solved then
                            log.Info "SOLVED!!! yay"
                            continueTrying <- false
                        else 
                            log.Warn "COuldn't solve with genlen %d, increasing" curGenLen
                    with exn ->
                        log.Error "FAILED: %A" exn
                    curGenLen <- curGenLen + 1
                if continueTrying then
                    log.Error "failed to solve, sorry :("
                else
                    log.Info "YESSSSSS"

        | "hungarianmode" :: minLevel :: [] ->
            let level = Int32.Parse minLevel
            let problems = Mtype.GenCache.loadProblems ()

            let estimateLevel (problem : Problem) =
                let ops = Set.remove "bonus" <| Set.ofSeq problem.operators
                if Set.contains "tfold" ops then
                    (Seq.length ops) * 2 - 4
                else
                    (Seq.length ops) * 2

            let pickOps (ops : string list) n =
                let rnd = new System.Random()
                ops 
                |> List.sortBy (fun op ->
                    match op with
                    | "tfold" -> 10
                    | "fold" -> 9
                    | "if0" -> 8
                    | "plus" | "xor" -> 7
                    | "not" | "shr1" | "shl1" -> 6
                    | "or" | "and" -> 5
                    | "shr1" | "shr4" | "shr16" -> 4
                    | _ -> 0)
                |> List.rev
                |> Seq.take (min n (Seq.length ops))
                |> Seq.sortBy (fun _ -> rnd.Next())
                |> List.ofSeq

            let rnd = System.Random()
            for p in problems |> Seq.filter (fun p -> estimateLevel p > level && (not p.solved)) |> Seq.sortBy (fun _ -> rnd.Next()) do
                let l = estimateLevel p
                log.Info "Problem %s is estimated to be of level %d" p.id l
                log.Debug "%A" p

                let haveTFold = Set.contains "tfold" <| Set.ofSeq p.operators
                let mutable curGenLen = 10
                let maxGenLen = if haveTFold then 15 else 13
                let mutable continueTrying = true
                while (curGenLen <= maxGenLen && continueTrying) do
                    let ops = pickOps (p.operators |> List.ofSeq) (if haveTFold then 6 else 5)
                    log.Info "Attempting to solve with gensize = %d and ops = %A" curGenLen ops
                    try
                        let solved = Game.Requests.solveProblem { p with size = curGenLen; operators = (Seq.ofList ops) } (Some 1080)
                        if solved then
                            log.Info "SOLVED!!! yay"
                            continueTrying <- false
                        else 
                            log.Warn "COuldn't solve with genlen %d, increasing" curGenLen
                    with exn ->
                        log.Error "FAILED: %A" exn
                    curGenLen <- curGenLen + 1
                if continueTrying then
                    log.Error "failed to solve, sorry :("
                else
                    log.Info "YESSSSSS"

        | [ "solve"; problemId ] ->
            log.Info "Loading problem %s..." problemId
            let problem = Mtype.GenCache.loadProblem problemId
            if Game.Requests.isProgramSolved problem then 
                log.Info "The problem is already solved."
            else                 
                Game.Requests.solveProblem problem None |> ignore

        | [ "solvehack"; genLen; problemId ] ->
            let genLen = Int32.Parse genLen
            log.Warn "HACK mode -- limiting generation by length %d" genLen
            log.Info "Loading problem %s..." problemId
            let problem = Mtype.GenCache.loadProblem problemId
            log.Info "Original problem SIZE = %d" problem.size
            if Game.Requests.isProgramSolved problem then 
                log.Info "The problem is already solved."
            else                 
                Game.Requests.solveProblem { problem with size = genLen } None |> ignore

        | [ "solvebysize"; size ] ->
            let size = Int32.Parse size
            log.Info "Loading problems of size %d..." size
            let probs = GenCache.loadProblems () |> List.filter (fun x -> x.size = size)
            log.Info "Loaded %d problems" probs.Length

            for (idx, prob) in List.mapi (fun i v -> (i, v)) probs do
                log.Info "Solving problem %d of %d" idx probs.Length
                if Game.Requests.isProgramSolved prob then 
                    log.Info "The problem %s is already solved." prob.id
                else                 
                    Game.Requests.solveProblem prob None |> ignore
    
        | [ "solvebysize"; size; maxOpOrders ] ->
            let size = Int32.Parse size
            let maxOpOrders = Int32.Parse maxOpOrders
            log.Info "Loading problems of size %d, generating limit - %d oporders..." size maxOpOrders
            let probs = GenCache.loadProblems () |> List.filter (fun x -> x.size = size)
            log.Info "Loaded %d problems" probs.Length

            for (idx, prob) in List.mapi (fun i v -> (i, v)) probs do
                log.Info "Solving problem %d of %d" idx probs.Length
                if Game.Requests.isProgramSolved prob then 
                    log.Info "The problem %s is already solved." prob.id
                else                 
                    Game.Requests.solveProblem prob (Some maxOpOrders) |> ignore
    
        0 // return an integer exit code
