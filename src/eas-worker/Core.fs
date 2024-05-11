module internal KdmidScheduler.Worker.Core

module private WorkerHandlers =
    module Serbia =
        let BelgradeSteps: Worker.Domain.Core.TaskHandler list =
            [ { Name = "Belgrade"
                Handle = None
                Steps =
                  [ { Name = "RussianEmbassy"
                      Handle = None
                      Steps =
                        [ { Name = "GetAvailableDates"
                            Handle = None
                            Steps = [] }
                          { Name = "NotifyUsers"
                            Handle = None
                            Steps = [] } ] }
                    { Name = "HungarianEmbassy"
                      Handle = None
                      Steps = [] } ] } ]

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Serbia"
        Handle = None
        Steps = WorkerHandlers.Serbia.BelgradeSteps } ]

let configure () =
    async {
        match! Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let config: Worker.Domain.Configuration =
                { getTask = Repository.getWorkerTask
                  Tasks = tasks
                  Handlers = handlers }

            return Ok config
    }
