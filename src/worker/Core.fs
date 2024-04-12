module Core

open Worker.Domain.Core

let getWorkerConfig args =
  async {

    match Repository.getTaskSettings () with
    | Error error -> $"Error: {error}" |> Logger.error
    | Ok tasks ->
    
    let duration =
      match args.Length with
      | 1 ->
      match args.[0] with
      | DSL.AP.IsFloat seconds -> seconds
      | _ -> (System.TimeSpan.FromDays 1).TotalSeconds
      | _ -> (System.TimeSpan.FromDays 1).TotalSeconds
      
      let config: Domain.Core.WorkerConfiguration =
        { Duration = duration
        Tasks = tasks
        Handlers = Core.taskHandlers
        getTask = Repository.getTask }
  }
        
module ExternalTask =
    let step_1 () = async { return Ok "Data received" }

    module Step1 =
        let step_1_1 () = async { return Ok "Data locked" }
        let step_1_2 () = async { return Ok "Data processed" }

    let step_2 () = async { return Ok "Data sent" }

let taskHandlers =
    [ { Name = "ExternalTask"
        Steps =
          [ { Name = "Step_1"
              Handle = ExternalTask.step_1
              Steps =
                [ { Name = "Step_1_1"
                    Handle = ExternalTask.Step1.step_1_1
                    Steps = [] }
                  { Name = "Step_1_2"
                    Handle = ExternalTask.Step1.step_1_2
                    Steps = [] } ] }
            { Name = "Step_2"
              Handle = ExternalTask.step_2
              Steps = [] }
            { Name = "Step_3"
              Handle = ExternalTask.step_2
              Steps =
                [ { Name = "Step_3_1"
                    Handle = ExternalTask.step_1
                    Steps = [] }
                  { Name = "Step_3_2"
                    Handle = ExternalTask.step_2
                    Steps = [] } ] } ] } ]
