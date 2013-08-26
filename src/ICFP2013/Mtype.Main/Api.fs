namespace Mtype

open System
open System.Net

module Data =
    open Newtonsoft

    type Problem = {
        id : string
        size : int
        operators : string seq
        solved : bool
        timeLeft : double
    }

    type EvalRequest = {
        id : string
        program : string
        arguments : string seq
    }

    type EvalResponse = {
        status : string
        outputs : string seq
        message : string
    }

    type GuessRequest = { id : string; program : string }
    type GuessResponse' = { status : string; values : string seq; message : string }
    type GuessValues = { input : string; their : string; ours : string }
    type GuessResponse = Win | Mismatch of (string * string * string) | Error of string

    type TrainRequest = { size : int; operators : string seq }
    type TrainingProblem = { challenge : string; id : string; size : int; operators : string seq }

    type RequestWindow = { resetsIn : double; amount : double; limit : int }
    type CpuWindow = { resetsIn : double; amount : double; limit : int }
    type StatusResponse = {
        easyChairId : int;
        contestScore : int;
        lightningScore : int;
        trainingScore : int;
        mismatches : int;
        numRequests : int;
        requestWindow : RequestWindow;
        cpuWindow : CpuWindow;
        cpuTotalTime : double;
    }

    let private jsonSettings = new Json.JsonSerializerSettings(
        NullValueHandling = Json.NullValueHandling.Ignore)

    let toJson<'a> (x : string) = Json.JsonConvert.DeserializeObject<'a> x
    let toJObject = toJson<Json.Linq.JObject>
    let parse<'a> x = Json.JsonConvert.DeserializeObject<'a> x
    let print (o : 'a) = Json.JsonConvert.SerializeObject(o :> obj, jsonSettings)

module Api =
    open Data
    let private log = Log.log "api"

    type ApiResponse<'a> =
        | Ok of 'a
        | TryAgainLater
        | BadRequest
        | Unauthorized
        | AuthRequired
        | NotFound
        | Gone
        | AlreadySolved
        | RequestTooBig
        | Fail of int option * Exception
        with
            member x.Value with 
                get () =
                match x with
                | Ok v -> v
                | _ -> failwith "not a success"

    let private authToken = "0109BjyZuoKNezhga48iBdPXGe4nCtvoOeBytzrF"
    let private httpClient = RestSharp.RestClient("http://icfpc2013.cloudapp.net/")
    let private timeoutMsec = 5000

    let private asyncExec (client : RestSharp.IRestClient) request =
        Async.AwaitTask <|
            let t = System.Threading.Tasks.TaskCompletionSource<_>()
            client.ExecuteAsync(request, (fun resp requestAsyncHandle ->
                t.SetResult(resp))) |> ignore
            t.Task

    let rec private request' meth mapfn body = async {
        log.Trace "--> POST %s/ %s" meth (log.Limit body 140)
        try
            let req = RestSharp.RestRequest(meth + "?auth=" + authToken + "vpsH1H", RestSharp.Method.POST)
            ignore <| req.AddParameter("application/json", body, RestSharp.ParameterType.RequestBody)
            req.Timeout <- timeoutMsec

            let! resp = asyncExec httpClient req

            if resp.ResponseStatus = RestSharp.ResponseStatus.TimedOut then
                raise <| System.TimeoutException()

            let code = int resp.StatusCode
            if code = 200 then
                let resp = resp.Content
                log.Trace "<== 200 %s" (log.Limit resp 140)
                return Ok (mapfn resp)
            else
                log.Trace "<== %d /%A/ %s (%s) exn: %A" code resp.ResponseStatus resp.Content resp.ErrorMessage resp.ErrorException
                match code with
                | 400 -> return BadRequest
                | 401 -> return Unauthorized
                | 403 -> return AuthRequired
                | 404 -> return NotFound
                | 410 -> return Gone
                | 412 -> return AlreadySolved
                | 413 -> return RequestTooBig
                | 429 -> return TryAgainLater 
                | code -> return Fail (Some code, resp.ErrorException)
        with
            | :? System.TimeoutException as exn ->
                log.Warn "<.. request timed out!"
                do! Async.Sleep 1000
                log.Trace "waiting before retry..."
                return! request' meth mapfn body
            | exn -> 
                log.Trace "<=! ??? exn: %A" exn
                return Fail (None, exn)
    }

    let request m b = request' m id b

    let myproblems () = request' "myproblems" parse<Problem seq> ""

    let evalProgram program arguments =
        request' "eval" parse<EvalResponse> <| 
            print { EvalRequest.id = null; program = program; arguments = arguments }

    let evalById id arguments =
        request' "eval" parse<EvalResponse> <| 
            print { EvalRequest.id = id; program = null; arguments = arguments }

    let train size operators =
        request' "train" parse<TrainingProblem> <| print { TrainRequest.size = size; operators = operators }

    let status () =
        request' "status" parse<StatusResponse> ""

    let guess progId program =
        request' "guess" (fun r ->
            let r = parse<GuessResponse'> r
            match r.status with
            | "win" -> Win
            | "mismatch" -> 
                let [input; their; ours] = List.ofSeq r.values
                Mismatch (input, their, ours)
            | "error" -> Error r.message
            | _ -> failwith "Wrong guess response")
        <| print { GuessRequest.id = progId; program = program }
