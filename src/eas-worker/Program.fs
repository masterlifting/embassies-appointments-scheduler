open Infrastructure
open Infrastructure.Domain.Graph
open Worker.Domain.Core

[<EntryPoint>]
let main _ =
    let workerName = "Scheduler"

    let rootTaskHandler =
        { Name = workerName
          Handle =
            Some
            <| fun _ -> async { return Ok <| TaskResult.Warn "Embassies Appointments Scheduler is running." } }

    let taskHandlersGraph =
        Node(
            rootTaskHandler,
            [ Eas.Worker.Core.Countries.Serbia.Handler
              Eas.Worker.Core.Countries.Bosnia.Handler
              Eas.Worker.Core.Countries.Montenegro.Handler
              Eas.Worker.Core.Countries.Hungary.Handler
              Eas.Worker.Core.Countries.Albania.Handler ]
        )

    let configuration = Configuration.get <| Configuration.File.Yaml "appsettings"

    Logging.useConsole configuration

    let getTaskNode = Eas.Worker.Data.getTaskNode taskHandlersGraph configuration

    Worker.Core.start workerName getTaskNode |> Async.RunSynchronously

    0
