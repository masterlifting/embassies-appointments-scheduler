open Infrastructure
open Infrastructure.Domain.Graph
open Worker.Domain.Internal

[<EntryPoint>]
let main _ =
    let workerName = "Scheduler"

    let rootNode =
        { Name = workerName
          Handle =
            Some
            <| fun _ -> async { return Ok <| Success "Embassies Appointments Scheduler is running..." } }

    let handlersGraph =
        Node(
            rootNode,
            [ Eas.Worker.Countries.Serbia.Node
              Eas.Worker.Countries.Bosnia.Node
              Eas.Worker.Countries.Montenegro.Node
              Eas.Worker.Countries.Hungary.Node
              Eas.Worker.Countries.Albania.Node ]
        )

    let configuration = Configuration.get <| Configuration.File.Yaml "appsettings"

    Logging.useConsole configuration

    let getTaskNode = Eas.Worker.Persistence.getTaskNode handlersGraph configuration

    Worker.Core.start <| workerName <| getTaskNode |> Async.RunSynchronously

    0
