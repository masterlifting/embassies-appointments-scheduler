module internal Eas.Worker.Persistence

open Infrastructure
open Infrastructure.Domain.Errors

[<Literal>]
let private sectionName = "Worker"

let private getTasksGraph handlersGraph configuration =
    Configuration.getSection<Worker.Domain.External.Task> configuration sectionName
    |> Option.map (fun graph -> Worker.Mapper.buildCoreGraph graph handlersGraph |> Result.mapError Persistence)
    |> Option.defaultValue (
        Error
        <| Persistence $"Section '%s{sectionName}' was not found in the configuration."
    )

let getTaskNode handlersGraph configuration =
    fun taskName ->
        async {
            return
                getTasksGraph handlersGraph configuration
                |> Result.bind (fun graph ->
                    Dsl.Graph.findNode taskName graph
                    |> Option.map Ok
                    |> Option.defaultValue (
                        Error
                        <| Persistence
                            $"Task '%s{taskName}' was not found in the section '%s{sectionName}' of the configuration."
                    ))
        }
