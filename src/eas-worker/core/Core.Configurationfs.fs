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
        let workerName = "EmbassiesAppointmentsScheduler"

        match! Data.getWorkerTasks workerName with
        | Error error -> return Error error
        | Ok tasks ->
            return
                Ok
                    { Name = workerName
                      getSchedule = Data.getTaskSchedule workerName
                      Tasks = tasks
                      Handlers = handlers }
    }
