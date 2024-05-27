open Infrastructure
open Eas.Worker
open Eas.Worker.Core.Configuration

[<EntryPoint>]
let main _ =
    Logging.useConsole Configuration.AppSettings

    Worker.Core.start RootName getTaskNode |> Async.RunSynchronously

    0
