namespace Mtype

open System
open Newtonsoft

module CLI =
    open GenCache
    let private log = Log.log "gencacheCLI"
        
    [<EntryPoint>]
    let main argv =
        match argv with
        | [| "getproblems" |] ->
            getProblems' () |> ignore
        | [| "getproblems"; "-f" |] ->
            getProblems ()
        | [| "showproblems" |] ->
            if System.IO.File.Exists(problemsPath) then
                let probs = loadProblems ()
                log.Info "Total %d problems:\n %s" probs.Length (printAsJson probs)
            else
                log.Info "No saved problems found. Use 'getproblems'."
        | [| "show"; pid |] ->
            let prob = loadProblem pid
            log.Info "Problem: %A" prob
            match loadPrograms pid with
            | Some ps ->
                log.Info "Total %d programs:" (Seq.length ps)
                ps |> Seq.iteri (fun idx p -> log.Info "- %d: %s" idx (BV.printExpr p))
            | None -> 
                log.Info "No cached programs found. Use 'gen <id>' to generate."
        | [| "gen"; pid |] ->
            let prob = loadProblem pid
            log.Info "Problem %s" pid
            if not (generatePrograms' prob) then
                log.Info "Programs are cached. Use 'gen -f <id>' to force re-generation."
            else ()
        | [| "genold"; pid |] ->
            let prob = loadProblem pid
            log.Info "Problem %s" pid
            generateProgramsOld prob
        | [| "gen"; "-f"; pid |] ->
            let prob = loadProblem pid
            log.Info "Problem %s" pid
            generatePrograms prob
        | [| "genforsize"; size |] ->
            let size = Int32.Parse size
            log.Info "Generating programs for problems of size %d" size
            let probs = loadProblems () |> List.filter (fun x -> x.size = size)

            for (idx, prob) in List.mapi (fun i x -> (i, x)) probs do
                log.Info "Processing problem %d of %d" (idx + 1) probs.Length
                if not (generatePrograms' prob) then log.Info "Skipping -- already in cache."
                else ()

//            [
//                for (idx, prob) in List.mapi (fun i x -> (i, x)) probs -> async {
//                        log.Info "Processing problem %d of %d" (idx + 1) probs.Length
//                        log.Info "Problem: %A" prob
//                        if not (generatePrograms' prob) then
//                            log.Info "Skipping -- already in cache."
//                            return ()
//                        else return ()
//                    }
//            ] |> Async.Parallel |> Async.RunSynchronously |> ignore
            
        | _ -> failwith "incorrect usage."
            
        0 // return an integer exit code
