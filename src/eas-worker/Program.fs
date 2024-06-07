open Infrastructure
open Infrastructure.Domain.Graph
open Worker.Domain.Internal

[<EntryPoint>]
let main _ =
    let workerName = "Scheduler"

    let rootHandler =
        { Name = workerName
          Handle =
            Some
            <| fun _ -> async { return Ok <| Warn "Embassies Appointments Scheduler is running..." } }

    let handlersGraph =
        Node(
            rootHandler,
            [ Eas.Worker.Core.Countries.Serbia.Handler
              Eas.Worker.Core.Countries.Bosnia.Handler
              Eas.Worker.Core.Countries.Montenegro.Handler
              Eas.Worker.Core.Countries.Hungary.Handler
              Eas.Worker.Core.Countries.Albania.Handler ]
        )

    let configuration = Configuration.get <| Configuration.File.Yaml "appsettings"

    Logging.useConsole configuration

    let getTaskNode = Eas.Worker.Data.getTaskNode handlersGraph configuration

    Worker.Core.start <| workerName <| getTaskNode |> Async.RunSynchronously

    0
