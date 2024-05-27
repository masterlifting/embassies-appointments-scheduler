module internal Eas.Worker.Data

open Infrastructure.Domain.Errors

[<Literal>]
let private sectionName = "Worker"


let private getTasksGraph handlersGraph =
    async {
        return
            match Configuration.getSection<Worker.Domain.Persistence.Task> sectionName with
            | None -> Error <| PersistenceError $"Section '%s{sectionName}' was not found."
            | Some graph -> Ok <| Worker.Mapper.buildCoreGraph graph handlersGraph
    }

let getTaskNode handlersGraph =
    fun taskName ->
        async {
            match! getTasksGraph handlersGraph with
            | Error error -> return Error error
            | Ok graph ->
                return
                    match Infrastructure.DSL.Graph.findNode taskName graph with
                    | Some node -> Ok node
                    | None ->
                        Error
                        <| PersistenceError $"Task '{taskName}' was not found in the section '{sectionName}'."
        }
