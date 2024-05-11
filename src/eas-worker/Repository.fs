module internal KdmidScheduler.Worker.Repository

[<Literal>]
let private WorkerSectionName = "EmbassiesAppointmentsScheduler"

let getWorkerTasks () =
    async {
        let sectionResult =
            Configuration.getSection<Worker.Domain.Persistence.Task array> WorkerSectionName

        return
            match sectionResult with
            | None -> Error $"Worker section '%s{WorkerSectionName}' was not found for the worker."
            | Some tasks -> Ok <| Array.map Worker.Mapper.mapToTask tasks
    }

let getWorkerTask name =
    async {
        let! tasksResult = getWorkerTasks ()

        return
            match tasksResult with
            | Error error -> Error error
            | Ok tasks ->
                match tasks |> Seq.tryFind (fun x -> x.Name = name) with
                | None -> Error $"Task '%s{name}' was not found in the section '%s{WorkerSectionName}' of the worker."
                | Some task -> Ok task
    }
