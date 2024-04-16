module Core

module private KdmidWorkerTask =
    open Worker.Domain.Core
    let step_1 () = async { return Ok "Data received" }

    module Step1 =
        let step_1_1 () = async { return Ok "Data locked" }
        let step_1_2 () = async { return Ok "Data processed" }

    let step_2 () = async { return Ok "Data sent" }

    let Handlers =
        [ { Name = "ExternalTask"
            Steps =
              [ { Name = "Step_1"
                  Handle = step_1
                  Steps =
                    [ { Name = "Step_1_1"
                        Handle = Step1.step_1_1
                        Steps = [] }
                      { Name = "Step_1_2"
                        Handle = Step1.step_1_2
                        Steps = [] } ] }
                { Name = "Step_2"
                  Handle = step_2
                  Steps = [] }
                { Name = "Step_3"
                  Handle = step_2
                  Steps =
                    [ { Name = "Step_3_1"
                        Handle = step_1
                        Steps = [] }
                      { Name = "Step_3_2"
                        Handle = step_2
                        Steps = [] } ] } ] } ]

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

            Configuration.appSettings |> Infrastructure.Logging.useConsoleLogger

            let config: Worker.Domain.Configuration =
                { Duration = duration
                  Tasks = tasks
                  Handlers = KdmidWorkerTask.Handlers
                  getTask = Repository.getTask }

            return Ok config
    }
