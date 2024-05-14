module internal KdmidScheduler.Worker.Core

open Infrastructure.Domain
open Worker.Domain.Core

module private WorkerHandlers =
    module Serbia =

        let BelgradeSteps: Graph<TaskHandler> list =
            [ Graph(
                  { Name = "Belgrade"; Handle = None },
                  [ Graph(
                        { Name = "RussianEmbassy"
                          Handle = None },
                        [ Graph(
                              { Name = "GetAvailableDates"
                                Handle = None },
                              []
                          )
                          Graph({ Name = "NotifyUsers"; Handle = None }, []) ]
                    )
                    Graph(
                        { Name = "HungarianEmbassy"
                          Handle = None },
                        [ Graph(
                              { Name = "GetAvailableDates"
                                Handle = None },
                              []
                          )
                          Graph({ Name = "NotifyUsers"; Handle = None }, []) ]
                    ) ]
              ) ]

let private handlers: Graph<TaskHandler> list =
    [ Graph({ Name = "Serbia"; Handle = None }, WorkerHandlers.Serbia.BelgradeSteps) ]

let configure () =
    async {
        match! Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let config: Worker.Domain.Configuration =
                { getSchedule = Repository.getTaskSchedule
                  Tasks = tasks
                  Handlers = handlers }

            return Ok config
    }
