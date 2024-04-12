[<EntryPoint>]
let main args =
    let config = args |> Core.getWorkerConfig |> Async.RunSynchronously
    config |> Core.startWorker |> Async.RunSynchronously
    0
