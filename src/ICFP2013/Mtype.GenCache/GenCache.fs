namespace Mtype

open System
open Newtonsoft

module GenCache =
    let private log = Log.log "gencache"

    type Programs = BV.Expr []

    let cacheRoot = @"..\..\..\data\problemcache"
    let programsPath pid = System.IO.Path.Combine(cacheRoot, pid)
    let problemsPath = System.IO.Path.Combine(cacheRoot, "problems")

    let cachedProgramsExist pid = System.IO.File.Exists(programsPath pid)
    let cachedProblemsExist () = System.IO.File.Exists(problemsPath)

    let loadPrograms problemId : Programs option =
        let path = programsPath problemId
        if cachedProgramsExist problemId then
            Some <| Persistence.loadFromFile<Programs> path 
        else None

    let savePrograms programs pid =
        Persistence.saveToFile programs (programsPath pid)

    let loadProblems () : Data.Problem list =
        Persistence.loadFromFile<Data.Problem list> problemsPath
    
    let loadProblem pid : Data.Problem =
        let probs = loadProblems ()
        probs |> List.find (fun x -> x.id = pid)

    let printAsJson obj =
        Json.JsonConvert.SerializeObject(obj, Json.Formatting.Indented)

    let generatePrograms (prob : Data.Problem) =
        log.Info "Generating programs for %A..." prob
        let len, ops = (prob.size - 1), (prob.operators |> Seq.map Gen.Op.Parse |> set)
        let ps = NewGen.emitForLengthAndOpset (len, ops) |> List.ofSeq
        log.Info "Generated %d programs." ps.Length
        log.Info "Saving..."
        savePrograms ps prob.id
        log.Info "Ok."

    let generateProgramsOld (prob : Data.Problem) =
        log.Info "Generating programs for %A..." prob
        let len, ops, vars = (prob.size - 1), (prob.operators |> Seq.map Gen.Op.Parse |> set), set ["x"]
        let ps = Gen.emit (len, ops, vars) |> List.ofSeq
        log.Info "Generated %d programs." ps.Length
        log.Info "Saving..."
        savePrograms ps prob.id
        log.Info "Ok."

    let generatePrograms' (prob : Data.Problem) =
        if cachedProgramsExist prob.id then false
        else 
            generatePrograms prob
            true

    let getProblems () =
        log.Info "Getting problem index..."
        let (Api.Ok probs) = Api.myproblems () |> Async.RunSynchronously
        let probs = probs |> List.ofSeq |> List.sortBy (fun (x : Data.Problem) -> x.size)
        log.Info "Total %d problems." probs.Length
        log.Info "Saving..."
        Persistence.saveToFile probs problemsPath
        log.Info "Ok."

    let getProblems' () =
        if cachedProblemsExist () then false
        else 
            getProblems ()
            true


