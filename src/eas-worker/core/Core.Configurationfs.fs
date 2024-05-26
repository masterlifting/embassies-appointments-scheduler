module internal Eas.Worker.Core.Configuration

open Eas.Worker
open Worker.Domain

let private handlers =
    [ Countries.Serbia.Handler
      Countries.Bosnia.Handler
      Countries.Montenegro.Handler
      Countries.Hungary.Handler
      Countries.Albania.Handler ]

let configure () =
    async {
        match! Data.getTasksGraph () with
        | Error error -> return Error error.Message
        | Ok tasksGraph ->
            return
                Ok
                    { TasksGraph = tasksGraph
                      Handlers = handlers
                      getTaskNode = Data.getTask }
    }
