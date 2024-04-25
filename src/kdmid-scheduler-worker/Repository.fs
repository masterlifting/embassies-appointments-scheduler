module internal KdmidScheduler.Worker.Repository

[<Literal>]
let private workerSection = "KdmidQueueChecker"

let getWorkerTasks () =
    async {
        let sectionResult =
            Configuration.getSection<Worker.Domain.Settings.Section> workerSection

        return
            match sectionResult with
            | None -> Error $"Worker section '%s{workerSection}' was not found for the worker."
            | Some section -> Ok <| (section.Tasks |> Seq.map (fun x -> Worker.Mapper.toTask x.Key x.Value))
    }

let getWorkerTask name =
    async {
        let! tasksResult = getWorkerTasks ()

        return
            match tasksResult with
            | Error error -> Error error
            | Ok tasks ->
                match tasks |> Seq.tryFind (fun x -> x.Name = name) with
                | None -> Error $"Task '%s{name}' was not found in the section '%s{workerSection}' of the worker."
                | Some task -> Ok task
    }
