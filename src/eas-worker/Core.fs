module internal KdmidScheduler.Worker.Core

open Infrastructure.Domain.Graph
open Worker.Domain.Core

module private WorkerHandlers =
    open System.Threading
    open Infrastructure.Domain.Errors

    module Serbia =

        let handleRussianEmbassy (cToken: CancellationToken) = async { return Ok "That's all" }

        let handleHungarianEmbassy (cToken: CancellationToken) =
            async {
                return Error(Logical NotImplemented)
            }

        let BelgradeSteps =
            [ Node(
                  { Name = "Belgrade"; Handle = None },
                  [ Node(
                        { Name = "RussianEmbassy"
                          Handle = Some handleRussianEmbassy },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = Some handleRussianEmbassy },
                              []
                          )
                          Node(
                              { Name = "NotifyUsers"
                                Handle = Some handleRussianEmbassy },
                              []
                          ) ]
                    )
                    Node(
                        { Name = "HungarianEmbassy"
                          Handle = None },
                        [ Node(
                              { Name = "GetAvailableDates"
                                Handle = None },
                              []
                          )
                          Node(
                              { Name = "NotifyUsers"
                                Handle = None },
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
