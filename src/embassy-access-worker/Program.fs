open Infrastructure
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Worker

[<EntryPoint>]
let main _ =
    let workerName = "Scheduler"

    let rootNode =
        { Name = workerName
          Handle =
            Some
            <| fun _ _ -> async { return Ok <| Success "Embassies Appointments Scheduler is running..." } }

    let handlersGraph =
        Node(
            rootNode,
            [ Countries.Serbia.Node
              Countries.Bosnia.Node
              Countries.Montenegro.Node
              Countries.Hungary.Node
              Countries.Albania.Node ]
        )

    let configuration = Configuration.get <| Configuration.File.Yaml "appsettings"

    Logging.useConsole configuration

    let getTaskNode = Task.getNode handlersGraph configuration

    Worker.Core.start <| workerName <| getTaskNode <| configuration
    |> Async.RunSynchronously

    0
