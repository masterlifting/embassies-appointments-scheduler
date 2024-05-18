module internal KdmidScheduler.Worker.Core

open Infrastructure.Domain.Graph
open Worker.Domain.Core

module private WorkerHandlers =
    module Serbia =
        let BelgradeSteps =
            [ Node(
                  { Name = "Belgrade"; Handle = None },
                  [ Node(
                        { Name = "RussianEmbassy"
                          Handle = None },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = None },
                              []
                          )
                          Node({ Name = "NotifyUsers"; Handle = None }, []) ]
                    )
                    Node(
                        { Name = "HungarianEmbassy"
                          Handle = None },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = None },
                              []
                          )
                          Node({ Name = "NotifyUsers"; Handle = None }, []) ]
                    ) ]
              ) ]

open Worker.Domain
open WorkerHandlers

let configure () =
    async {
        match! Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            return
                Ok
                <| { getSchedule = Repository.getTaskSchedule
                     Tasks = tasks
                     Handlers = [ Node({ Name = "Serbia"; Handle = None }, Serbia.BelgradeSteps) ] }
    }
