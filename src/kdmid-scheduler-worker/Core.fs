module KdmidScheduler.Worker.Core

module private WorkerHandlers =
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

                    let cityOrder: KdmidScheduler.Domain.Core.CityOrder =
                        { City = city
                          UserCredentials = credentials }

                    match! KdmidScheduler.Core.processCityOrder pScope cityOrder with
                    | Error error -> 
                        Persistence.Core.Scope.clear pScope
                        return Error error
                    | Ok _ ->
                        Persistence.Core.Scope.clear pScope
                        return Ok "Available dates were processed."

                    
        }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> KdmidScheduler.Domain.Core.Belgrade |> WorkerHandlers.getAvailableDates
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> KdmidScheduler.Domain.Core.Sarajevo |> WorkerHandlers.getAvailableDates
              Steps = [] } ] } ]

let configureWorker cToken =
    async {
        match! KdmidScheduler.Worker.Repository.getTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let config: Worker.Domain.Configuration =
                { CancellationToken = cToken
                  Tasks = tasks
                  getTask = KdmidScheduler.Worker.Repository.getTask
                  Handlers = handlers }

            return Ok config
    }
