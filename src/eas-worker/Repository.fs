module internal KdmidScheduler.Worker.Repository

[<Literal>]
let private WorkerSectionName = "EmbassiesAppointmentsScheduler"

let getWorkerTasks () =
    async {
        let sectionResult =
            Configuration.getSection<Worker.Domain.Persistence.Task array> WorkerSectionName

        return
            match sectionResult with
            | None -> Error $"Section '%s{WorkerSectionName}' was not found."
            | Some tasks -> Ok <| Worker.Mapper.mapTasks tasks
    }

let getTaskSchedule name =
    async {
        let! tasksResult = getWorkerTasks ()

        return
            match tasksResult with
            | Error error -> Error error
            | Ok tasks ->
                match Infrastructure.DSL.Graph.findNode'<Worker.Domain.Core.Task> name tasks with
                | Some task -> Ok task.Schedule
                | None -> Error $"Task '{name}' was not found in the section '{WorkerSectionName}'."
    }
