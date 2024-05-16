module internal KdmidScheduler.Worker.Repository

[<Literal>]
let private WorkerSectionName = "EmbassiesAppointmentsScheduler"

let getWorkerTasks () =
    async {
        match Configuration.getSection<Worker.Domain.Persistence.Task array> WorkerSectionName with
        | None -> return Error $"Section '%s{WorkerSectionName}' was not found."
        | Some tasks when tasks.Length = 0 -> return Error $"Section '%s{WorkerSectionName}' is empty."
        | Some tasks -> return Ok <| Worker.Mapper.mapTasks tasks
    }

let getTaskSchedule name =
    async {
        match! getWorkerTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            match Infrastructure.DSL.Graph.findNode' name tasks with
            | Some task -> return Ok task.Schedule
            | None -> return Error $"Task '{name}' was not found in the section '{WorkerSectionName}'."
    }
