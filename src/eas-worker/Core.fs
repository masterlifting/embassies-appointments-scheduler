module internal KdmidScheduler.Worker.Core

open Infrastructure.Domain.Graph
open Worker.Domain.Core

module private WorkerHandlers =
    open System.Threading

    module Serbia =

        let handleRusianEmbassy (cts: CancellationTokenSource) = async { return Ok "That's all" }

        let BelgradeSteps =
            [ Node(
                  { Name = "Belgrade"; Handle = None },
                  [ Node(
                        { Name = "RussianEmbassy"
                          Handle = Some handleRusianEmbassy },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = Some handleRusianEmbassy },
                              []
                          )
                          Node(
                              { Name = "NotifyUsers"
                                Handle = Some handleRusianEmbassy },
                              []
                          ) ]
                    )
                    Node(
                        { Name = "HungarianEmbassy"
                          Handle = Some handleRusianEmbassy },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = Some handleRusianEmbassy },
                              []
                          )
                          Node(
                              { Name = "NotifyUsers"
                                Handle = Some handleRusianEmbassy },
                              []
                          ) ]
                    ) ]
              ) ]

open Worker.Domain
open WorkerHandlers

let configure () =
    async {
        let workerName = "EmbassiesAppointmentsScheduler"

        match! Repository.getWorkerTasks workerName with
        | Error error -> return Error error
        | Ok tasks ->
            return
                Ok
                <| { Name = workerName
                     getSchedule = Repository.getTaskSchedule workerName
                     Tasks = tasks
                     Handlers = [ Node({ Name = "Serbia"; Handle = None }, Serbia.BelgradeSteps) ] }
    }
