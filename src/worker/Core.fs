module Core

open Worker.Domain.Core

module private KdmidTasks =
    open Infrastructure.Logging
    let step_1 () = async { return Ok "Data received" }

    module Step1 =
        let step_1_1 () = async { return Ok "Data locked" }
        let step_1_2 () = async { return Ok "Data processed" }

    let step_2 () = async { return Ok "Data sent" }

let private handlers =
    [ { Name = "ExternalTask"
        Steps =
          [ { Name = "Step_1"
              Handle = KdmidTasks.step_1
              Steps =
                [ { Name = "Step_1_1"
                    Handle = KdmidTasks.Step1.step_1_1
                    Steps = [] }
                  { Name = "Step_1_2"
                    Handle = KdmidTasks.Step1.step_1_2
                    Steps = [] } ] }
            { Name = "Step_2"
              Handle = KdmidTasks.step_2
              Steps = [] }
            { Name = "Step_3"
              Handle = KdmidTasks.step_2
              Steps =
                [ { Name = "Step_3_1"
                    Handle = KdmidTasks.step_1
                    Steps = [] }
                  { Name = "Step_3_2"
                    Handle = KdmidTasks.step_2
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

            let config: Worker.Domain.Configuration =
                { Duration = duration
                  Tasks = tasks
                  getTask = Repository.getTask
                  Handlers = handlers }

            Infrastructure.Logging.useConsoleLogger <| Configuration.appSettings

            return Ok config
    }
