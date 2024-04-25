module internal KdmidScheduler.Worker.Core

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

let configure (args: string array) =
    async {

        Infrastructure.Logging.useConsoleLogger <| Configuration.AppSettings

        match! Repository.getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            let seconds =
                match args.Length with
                | 1 ->
                    match args.[0] with
                    | Infrastructure.DSL.AP.IsFloat value -> value
                    | _ -> (System.TimeSpan.FromDays 1).TotalSeconds
                | _ -> (System.TimeSpan.FromDays 1).TotalSeconds


            let duration = System.TimeSpan.FromSeconds seconds
            use cts = new System.Threading.CancellationTokenSource(duration)

            $"The worker will be running for %d{duration.Days}d %02d{duration.Hours}h %02d{duration.Minutes}m %02d{duration.Seconds}s"
            |> Infrastructure.Logging.Log.warning

            let config: Worker.Domain.Configuration =
                { CancellationToken = cts.Token
                  getTask = Repository.getWorkerTask
                  Tasks = tasks
                  Handlers = handlers }

            return Ok config
    }
