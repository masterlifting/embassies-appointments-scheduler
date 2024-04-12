module Repository
open Configuration

let getTaskSettings () =
    let configuretionTasks = getSection<Worker.Domain.Settings.Section> "Worker"
    match configuretionTasks with
    | None-> Error "Worker configuration not found"
    | Some settings -> Ok settings.Tasks


let getTasks () =
    match getTaskSettings () with
    | Error error -> Error error
    | Ok tasks -> Ok (tasks |> List.map (fun task -> Worker.Mapper.toTask task.Key, task.Value))

let getTask name =
    match getTasks () with
    | Error error -> Error error
    | Ok tasks ->
        match tasks |> List.tryFind (fun (taskName, _) -> taskName = name) with
        | None -> Error $"Task '{name}' was not found"
        | Some (_, task) -> Ok task