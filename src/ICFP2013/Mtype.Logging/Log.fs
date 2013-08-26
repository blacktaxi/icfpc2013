namespace Mtype

[<AutoOpen>]
module LogImpl =
    open NLog

    type PrintfLog (name) =
        let log = NLog.LogManager.GetLogger name
        member internal x._Info (msg : string) = log.Info msg
        member internal x._Debug (msg : string) = log.Debug msg
        member internal x._Error (msg : string) = log.Error msg
        member internal x._Warn (msg : string) = log.Warn msg
        member internal x._Trace (msg : string) = log.Trace msg
        member internal x._Fatal (msg : string) = log.Fatal msg

        member __.Limit (str : string) maxLen =
            if str.Length > maxLen then str.Substring(0, maxLen) + "..."
            else str

    [<AutoOpen>]
    module Extensions =
        type PrintfLog with
            member x.Info format = Printf.kprintf x._Info format
            member x.Debug format = Printf.kprintf x._Debug format
            member x.Error format = Printf.kprintf x._Error format
            member x.Warn format = Printf.kprintf x._Warn format
            member x.Trace format = Printf.kprintf x._Trace format
            member x.Fatal format = Printf.kprintf x._Fatal format

module Log =
    let log name = new PrintfLog(name)    