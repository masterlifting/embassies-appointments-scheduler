module internal Eas.Worker.Data

open Infrastructure.Domain.Errors
open Infrastructure.Domain.Graph
open Infrastructure.Logging

let getTasks workerName =
    async {
        match Configuration.getSection<Worker.Domain.Persistence.Task array> workerName with
        | None ->
            return
                Error
                <| AppError.Infrastructure(PersistenceError $"Section '%s{workerName}' was not found.")
        | Some tasks -> return Ok <| Worker.Mapper.mapTasks tasks
    }

let rec getTask workerName =
    fun taskName ->
        async {
            match! getTasks workerName with
            | Error error ->
                error.Message |> Log.error
                return None
            | Ok tasks ->
                match Infrastructure.DSL.Graph.findNode' taskName tasks with
                | Some task ->
                    let! children = getTask workerName task.Name 
                    return Some <| Node (task, children)
                | None ->
                    $"Task '{taskName}' was not found in the section '{workerName}'." |> Log.warning
                    return None
        }
