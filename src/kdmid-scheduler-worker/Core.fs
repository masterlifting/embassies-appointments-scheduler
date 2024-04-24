module KdmidScheduler.Worker.Core

open KdmidScheduler

module private WorkerHandlers =
    let getAvailableDates city storage =
        async {
            match Persistence.Core.Scope.create storage with
            | Error error -> return Error error
            | Ok pScope ->
                match! Repository.getUserCredentials pScope city with
                | Error error -> return Error error
                | Ok credentials ->

                    let cityOrder: Domain.Core.CityOrder =
                        { City = city
                          UserCredentials = credentials }

                    match! Core.processCityOrder pScope cityOrder with
                    | Error error -> return Error error
                    | Ok _ -> return Ok "Available dates were processed"
        }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> WorkerHandlers.getAvailableDates Domain.Core.Belgrade Persistence.Core.InMemoryStorage
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> WorkerHandlers.getAvailableDates Domain.Core.Sarajevo Persistence.Core.InMemoryStorage
              Steps = [] } ] } ]

let configWorker (args: string array) =
    async {
        match! Repository.getTasks () with
        | Error error -> return Error error
        | Ok tasks ->

            let duration =
                match args.Length with
                | 1 ->
                    match args.[0] with
                    | Infrastructure.DSL.AP.IsFloat seconds -> seconds
                    | _ -> (System.TimeSpan.FromDays 1).TotalSeconds
                | _ -> (System.TimeSpan.FromDays 1).TotalSeconds

            let config: Worker.Domain.Configuration =
                { Duration = duration
                  Tasks = tasks
                  getTask = Repository.getTask
                  Handlers = handlers }

            Infrastructure.Logging.useConsoleLogger <| Configuration.appSettings

            return Ok config
    }
