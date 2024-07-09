module internal Eas.Worker.Persistence

open Infrastructure
open Infrastructure.Configuration
open Infrastructure.Domain.Errors

[<Literal>]
let private sectionName = "Worker"

let private getTasksGraph handlersGraph configuration =
    configuration
    |> getSection<Worker.Domain.External.Task> sectionName
    |> Option.map (fun graph -> Worker.Mapper.buildCoreGraph graph handlersGraph)
    |> Option.defaultValue (
        Error
        <| NotFound $"Section '%s{sectionName}' in the configuration."
    )

let getTaskNode handlersGraph configuration =
    fun taskName ->
        async {
            return
                getTasksGraph handlersGraph configuration
                |> Result.bind (fun graph ->
                    DSL.Graph.findNode taskName graph
                    |> Option.map Ok
                    |> Option.defaultValue (
                        Error
                        <| NotFound
                            $"Task '%s{taskName}' in the section '%s{sectionName}' of the configuration."
                    ))
        }
