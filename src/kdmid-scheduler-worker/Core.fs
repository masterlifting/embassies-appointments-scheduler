module internal KdmidScheduler.Worker.Core

open System
open Infrastructure
open Infrastructure.Domain
open Infrastructure.Domain.Errors
open KdmidScheduler.Domain.Core.Embassies

module private WorkerHandlers =
    open Persistence.Core
    open KdmidScheduler.Core

    let prepareRussianEmbassyFor (city: City) =
        async {
            let embassy = city |> Serbia |> Russian
            let! result = KdmidScheduler.Core.processEmbassy embassy

            match result with
            | Ok _ -> return Ok "Result: The Russian embassy was processed."
            | Error(LogicError NotImplemented) -> return Error "Not implemented."
            | Error _ -> return Error "Some error occurred."

        }

    let prepareHungarianEmbassyFor (city: City) =
        async { return Error <| LogicError NotImplemented }


    [<Literal>]
    let FindAvailableDatesStepName = "FindAvailableDates"

    [<Literal>]
    let PropagateFoundResultStepName = "PropagateFoundResult"

    let findAvailableDatesFor (city: City) =
        async {
            match Storage.create InMemory with
            | Error error -> return Error error
            | Ok storage ->
                match! User.getUserKdmidOrders city storage with
                | Error error -> return Error error
                | Ok None -> return Ok "Result: Data was not found."
                | Ok(Some orders) ->
                    match! Kdmid.getCredentialAppointments orders with
                    | Error(LogicError NotImplemented) -> return Error "Not implemented."
                    | Error _ -> return Error "Some error occurred."
                    | Ok results -> return Ok $"Result:\n{results}"
        }

    let propagateFoundResultFor city =
        async { return Error "propagateFoundResultFor is not implemented." }


let BelgradeSteps: Worker.Domain.Core.TaskStepHandler list =
    [ { Name = "ProcessRussianEmbassy"
        Handle = fun _ -> WorkerHandlers.prepareRussianEmbassyFor Belgrade
        Steps =
          [ { Name = "FindAvailableDates"
              Handle = fun _ -> WorkerHandlers.findAvailableDatesFor Belgrade
              Steps = [] }
            { Name = "PropagateFoundResult"
              Handle = fun _ -> WorkerHandlers.propagateFoundResultFor Belgrade
              Steps = [] } ] }
      { Name = "ProcessHungarianEmbassy"
        Handle = fun _ -> WorkerHandlers.prepareHungarianEmbassyFor Belgrade
        Steps = [] } ]

let private handlers: Worker.Domain.Core.TaskHandler list =
    [ { Name = "Belgrade"
        Steps = BelgradeSteps } ]

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
