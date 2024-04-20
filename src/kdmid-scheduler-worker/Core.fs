module KdmidScheduler.Worker.Core

open KdmidScheduler

module private WorkerHandlers =
    open Domain.Core

    let getAvailableDates city =
        async {
            match! Core.getUserCredentials city with
            | Error error -> return Error error
            | Ok userCredentials ->

                let cityOrder =
                    { City = city
                      UserCredentials = userCredentials }

                match! Core.getAvailableDates cityOrder with
                | Error error -> return Error error
                | Ok availableDates -> return Ok availableDates
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
