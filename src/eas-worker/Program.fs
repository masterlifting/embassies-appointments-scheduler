﻿open Infrastructure
open KdmidScheduler.Worker

[<EntryPoint>]
let main _ =
    Logging.useConsoleLogger Configuration.AppSettings
    
    //TODO: Remove this line
    //do! KdmidScheduler.Core.createTestUserKdmidOrderForCity Belgrade

    Core.configure
    |> Worker.Core.start
    |> Async.RunSynchronously

    Async.Sleep (System.TimeSpan.FromSeconds 0.5)
    |> Async.RunSynchronously

    0
