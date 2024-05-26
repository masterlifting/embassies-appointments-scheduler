open Infrastructure
open Eas.Worker
open Eas

[<EntryPoint>]
let main _ =
    Logging.useConsole Configuration.AppSettings

    let workerConfigRes =
        Worker.Core.Configuration.configure () |> Async.RunSynchronously

    match workerConfigRes with
    | Error error -> error |> Logging.Log.error
    | Ok workerConfig -> workerConfig |> Worker.Core.start |> Async.RunSynchronously

    0
