module internal KdmidScheduler.Worker.Repository


let getWorkerTasks workerName =
    async {
        match Configuration.getSection<Worker.Domain.Persistence.Task array> workerName with
        | None -> return Error $"Section '%s{workerName}' was not found."
        | Some tasks when tasks.Length = 0 -> return Error $"Section '%s{workerName}' is empty."
        | Some tasks -> return Ok <| Worker.Mapper.mapTasks tasks
    }

let getTaskSchedule workerName =
    fun taskName ->
        async {
            match! getWorkerTasks workerName with
            | Error error -> return Error error
            | Ok tasks ->
                match Infrastructure.DSL.Graph.findNode' taskName tasks with
                | Some task -> return Ok task.Schedule
                | None -> return Error $"Task '{taskName}' was not found in the section '{workerName}'."
    }
