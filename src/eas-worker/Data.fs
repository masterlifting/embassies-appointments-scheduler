module internal Eas.Worker.Data

open Infrastructure.Domain.Errors
open Infrastructure.Domain.Graph

[<Literal>]
let private sectionName = "Worker"


let getTasksGraph handlers =
    async {
        return
            match Configuration.getSection<Worker.Domain.Persistence.Task> sectionName with
            | None -> Error <| PersistenceError $"Section '%s{sectionName}' was not found."
            | Some task -> Ok <| Worker.Mapper.buildGraph task handlers
    }

let rec getTaskNode handlers =
    fun taskName ->
        async {
            match! getTasksGraph handlers with
            | Error error -> return Error error
            | Ok graph ->
                return
                    match Infrastructure.DSL.Graph.findNode taskName graph with
                    | Some task -> Ok task
                    | None ->
                        Error
                        <| PersistenceError $"Task '{taskName}' was not found in the section '{sectionName}'."
        }
