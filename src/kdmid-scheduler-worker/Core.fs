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
                | Ok(None) -> return Ok "Result: Data was not found."
                | Ok(Some userCredentials) ->

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

        match Persistence.Core.Storage.create Persistence.Core.Type.InMemory with
        | Error error -> return Error error
        | Ok storage ->

            let user: KdmidScheduler.Domain.Core.User = { Id = UserId "1"; Name = "John" }

            let kdmidCredentials =
                [| 1; 2 |]
                |> Seq.map (fun x -> KdmidScheduler.Domain.Core.Kdmid.createCredentials x (x |> string) None)
                |> Infrastructure.DSL.Seq.resultOrError

            match kdmidCredentials with
            | Error error -> return Error error
            | Ok kdmidCredentials ->

                let userCredentials: UserCredentials = Map [ user, kdmidCredentials |> set ]

                match! KdmidScheduler.Core.addUserCredentials Belgrade userCredentials storage with
                | Error error -> return Error error
                | Ok _ ->
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
