[<EntryPoint>]
let main args =
    match args |> Core.getWorkerConfig |> Async.RunSynchronously with
    | Error error ->
        error |> Logger.error
        1
    | Ok config ->
        config |> Worker.Core.startWorker |> Async.RunSynchronously
        0
