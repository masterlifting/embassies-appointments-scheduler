module Repository

let getTasks () =
    async {
        match Configuration.getSection<Worker.Domain.Settings.Section> "KdmidQueueChecker" with
        | None -> return Error "Worker section not found"
        | Some section -> return Ok <| (section.Tasks |> Seq.map (fun x -> Worker.Mapper.toTask x.Key x.Value))
    }

let getTask name =
    async {
        match! getTasks () with
        | Error error -> return Error error
        | Ok tasks ->
            match tasks |> Seq.tryFind (fun x -> x.Name = name) with
            | None -> return Error "Task not found"
            | Some task -> return Ok task
    }
