module internal EmbassyAccess.Worker.TasksStorage

open Infrastructure
open Infrastructure.Configuration

[<Literal>]
let private sectionName = "Worker"

let private buildGraph handlersGraph configuration =
    configuration
    |> getSection<Worker.Domain.External.Task> sectionName
    |> Option.map (fun graph -> Worker.Graph.build graph handlersGraph)
    |> Option.defaultValue (Error <| NotFound $"Section '%s{sectionName}' in the configuration.")

let getTask handlersGraph configuration =
    fun taskName ->
        async {
            return
                buildGraph handlersGraph configuration
                |> Result.bind (fun graph ->
                    DSL.Graph.findNode taskName graph
                    |> Option.map Ok
                    |> Option.defaultValue (
                        Error
                        <| NotFound $"Task '%s{taskName}' in the section '%s{sectionName}' of the configuration."
                    ))
        }
