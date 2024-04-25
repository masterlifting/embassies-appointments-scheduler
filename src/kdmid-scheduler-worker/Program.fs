[<EntryPoint>]
let main args =
    args
    |> KdmidScheduler.Worker.Core.configure
    |> Worker.Core.start
    |> Async.RunSynchronously

    0
