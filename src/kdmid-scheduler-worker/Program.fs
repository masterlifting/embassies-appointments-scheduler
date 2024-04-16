[<EntryPoint>]
let main args =
    args |> Core.configWorker |> Worker.Core.startWorker |> Async.RunSynchronously
    0
