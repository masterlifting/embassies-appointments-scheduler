module internal KdmidScheduler.Worker.Core

open System
open Infrastructure
open KdmidScheduler.Domain.Core.Kdmid

module private WorkerHandlers =
    open Persistence.Core
    open KdmidScheduler.Core

    [<Literal>]
    let FindAvailableDatesStepName = "FindAvailableDates"

    [<Literal>]
    let PropagateFoundResultStepName = "PropagateFoundResult"

    let findAvailableDatesFor (city: City) =
        async {
            match Storage.create InMemory with
            | Error error -> return Error error
            | Ok storage ->
                match! getUserKdmidOrdersByCity city storage with
                | Error error -> return Error error
                | Ok None -> return Ok "Result: Data was not found."
                | Ok(Some orders) ->
                    match! processUserKdmidOrders orders with
                    | Error error -> return Error error
                    | Ok results -> return Ok $"Result:\n{results}"
        }

    let propagateFoundResultFor city =
        async { return Error "propagateFoundResultFor is not implemented." }

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps =
          [ { Name = WorkerHandlers.FindAvailableDatesStepName
              Handle = fun _ -> WorkerHandlers.findAvailableDatesFor Belgrade
              Steps = [] }
            { Name = WorkerHandlers.PropagateFoundResultStepName
              Handle = fun _ -> WorkerHandlers.propagateFoundResultFor Belgrade
              Steps = [] } ] }
      { Name = "Sarajevo"
        Steps =
          [ { Name = WorkerHandlers.FindAvailableDatesStepName
              Handle = fun _ -> WorkerHandlers.findAvailableDatesFor Sarajevo
              Steps = [] } ] } ]

let configure (args: string array) =
    async {

        Logging.useConsoleLogger <| Configuration.AppSettings

        //TODO: Remove this line
        do! KdmidScheduler.Core.createTestUserKdmidOrderForCity Belgrade

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
