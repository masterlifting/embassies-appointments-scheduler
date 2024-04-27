module internal KdmidScheduler.Worker.Core

open System
open Infrastructure
open KdmidScheduler.Domain.Core

module private WorkerHandlers =
    open Persistence.Core
    open KdmidScheduler.Core

    [<Literal>]
    let FindAvailableDatesStepName = "FindAvailableDates"

    let findAvailableDatesFor city =
        async {
            match Storage.create Type.InMemory with
            | Error error -> return Error error
            | Ok storage ->
                match! getUserCredentials city storage with
                | Error error -> return Error error
                | Ok credentialsOpt ->

                    match credentialsOpt with
                    | None -> return Ok "Result: Data was not found."
                    | Some userCredentials ->

                        let order =
                            { City = city
                              UserCredentials = userCredentials }

                        match! processCityOrder order storage with
                        | Error error -> return Error error
                        | Ok result -> return Ok $"Result:\n{result}"
        }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = WorkerHandlers.FindAvailableDatesStepName
              Handle = fun _ -> WorkerHandlers.findAvailableDatesFor Belgrade
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = WorkerHandlers.FindAvailableDatesStepName
              Handle = fun _ -> WorkerHandlers.findAvailableDatesFor Sarajevo
              Steps = [] } ] } ]

let configure (args: string array) =
    async {

        Logging.useConsoleLogger <| Configuration.AppSettings

        match! Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let seconds =
                match args.Length with
                | 1 ->
                    match args[0] with
                    | DSL.AP.IsFloat value -> value
                    | _ -> (TimeSpan.FromDays 1).TotalSeconds
                | _ -> (TimeSpan.FromDays 1).TotalSeconds


            let duration = TimeSpan.FromSeconds seconds
            use cts = new Threading.CancellationTokenSource(duration)

            $"The worker will be running for %d{duration.Days}d %02d{duration.Hours}h %02d{duration.Minutes}m %02d{duration.Seconds}s"
            |> Logging.Log.warning

            let config: Worker.Domain.Configuration =
                { CancellationToken = cts.Token
                  getTask = Repository.getWorkerTask
                  Tasks = tasks
                  Handlers = handlers }

            return Ok config
    }
