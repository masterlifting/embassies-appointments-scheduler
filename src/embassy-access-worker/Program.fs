open Infrastructure
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open EmbassyAccess.Worker

[<EntryPoint>]
let main _ =

    let configuration = Configuration.getYaml "appsettings"
    Logging.useConsole configuration

    let rootNode =
        { Name = "Scheduler"
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

    "Scheduler"
    |> Worker.Core.start
        { getTask = Task.get handlersGraph configuration
          Configuration = configuration }
    |> Async.RunSynchronously

    0
