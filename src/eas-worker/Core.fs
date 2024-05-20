module internal KdmidScheduler.Worker.Core

open Infrastructure.Domain.Graph
open Worker.Domain.Core

module private WorkerHandlers =
    open System.Threading
    open Infrastructure.Domain.Errors

    module Serbia =

        let handleRusianEmbassy (cts: CancellationTokenSource) = async { return Ok "That's all" }

        let handleHungarianEmbassy (cts: CancellationTokenSource) =
            async {
                cts.Cancel()
                return Error(Logical NotImplemented)
            }

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
                          Handle = Some handleHungarianEmbassy },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = Some handleHungarianEmbassy },
                              []
                          )
                          Node(
                              { Name = "NotifyUsers"
                                Handle = Some handleHungarianEmbassy },
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
