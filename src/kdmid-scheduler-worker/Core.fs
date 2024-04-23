module KdmidScheduler.Worker.Core

open KdmidScheduler

module private WorkerHandlers =
    let getAvailableDates city =
        async {
            match Persistence.Core.Scope.create Persistence.Core.InMemoryStorage with
            | Error error -> return Error error
            | Ok persistenceScope ->
                match
                    Domain.Core.Kdmid.createCredentials
                        (Domain.Core.Kdmid.Id "1")
                        (Domain.Core.Kdmid.Cd "2")
                        (Domain.Core.Kdmid.Ems(Some "3"))
                with
                | Error error -> return Error error
                | Ok credential ->
                    let user: Domain.Core.User =
                        { Id = Domain.Core.UserId "1"
                          Name = "John Doe" }

                    let userCredentials: Domain.Core.UserCredentials = Map [ user, Set [ credential ] ]

                    match! Repository.addUserCredentials persistenceScope city userCredentials with
                    | Error error -> return Error error
                    | Ok _ ->

                        match! Repository.getUserCredentials persistenceScope city with
                        | Error error -> return Error error
                        | Ok userCredentials ->

                            let cityOrder: Domain.Core.CityOrder =
                                { City = city
                                  UserCredentials = userCredentials }

                            Infrastructure.Logging.Log.info $"City order: {cityOrder}"

                            match! Core.processCityOrder persistenceScope cityOrder with
                            | Error error -> return Error error
                            | Ok _ -> return Ok "Available dates were processed"
        }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> Domain.Core.Belgrade |> WorkerHandlers.getAvailableDates
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = fun _ -> Domain.Core.Sarajevo |> WorkerHandlers.getAvailableDates
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
