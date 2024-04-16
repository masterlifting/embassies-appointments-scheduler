module Core

open Worker.Domain.Core

module private KdmidQueueChecker =
    open Infrastructure.Logging

    let getAvailableDates () =
        async {
            let startPage = 1
            return Ok "Data received"
        }

let private handlers =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = "GetAvailableDates"
              Handle = KdmidQueueChecker.getAvailableDates
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
