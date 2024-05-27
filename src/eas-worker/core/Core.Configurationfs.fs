module internal Eas.Worker.Core.Configuration

open Eas.Worker
open Worker.Domain.Core
open Infrastructure.Domain.Graph

let private handlersGraph =
    Node(
        { Name = "Scheduler"
          Handle = Some(fun _ -> async { return Ok "Embassies Appointments Scheduler is running." }) },
        [ Countries.Serbia.Handler
          Countries.Bosnia.Handler
          Countries.Montenegro.Handler
          Countries.Hungary.Handler
          Countries.Albania.Handler ]
    )

let RootName = handlersGraph.Value.Name
let getTaskNode = Data.getTaskNode handlersGraph
