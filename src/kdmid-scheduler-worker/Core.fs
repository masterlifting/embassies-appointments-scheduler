module KdmidScheduler.Worker.Core

module private WorkerHandlers =
    [<Literal>]
    let getAvailableDatesStep = "GetAvailableDates"

    let getAvailableDates city =
        async {
            match Persistence.Core.Scope.create Persistence.Core.InMemoryStorage with
            | Error error -> return Error error
            | Ok pScope ->
                match! KdmidScheduler.Repository.getUserCredentials pScope city with
                | Error error ->
                    Persistence.Core.Scope.clear pScope
                    return Error error
                | Ok credentials ->

                    let order: KdmidScheduler.Domain.Core.CityOrder =
                        { City = city
                          UserCredentials = credentials }

                    match! KdmidScheduler.Core.processCityOrder pScope order with
                    | Error error ->
                        Persistence.Core.Scope.clear pScope
                        return Error error
                    | Ok result ->
                        Persistence.Core.Scope.clear pScope
                        return Ok $"Worker processed results for '{city}' are: \n{result}"
        }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = WorkerHandlers.getAvailableDatesStep
              Handle = fun _ -> KdmidScheduler.Domain.Core.Belgrade |> WorkerHandlers.getAvailableDates
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = WorkerHandlers.getAvailableDatesStep
              Handle = fun _ -> KdmidScheduler.Domain.Core.Sarajevo |> WorkerHandlers.getAvailableDates
              Steps = [] } ] } ]

let configure cToken =
    async {
        match! KdmidScheduler.Worker.Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let config: Worker.Domain.Configuration =
                { CancellationToken = cToken
                  getTask = KdmidScheduler.Worker.Repository.getWorkerTask
                  Tasks = tasks
                  Handlers = handlers }

            return Ok config
    }
