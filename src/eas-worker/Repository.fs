module internal KdmidScheduler.Worker.Repository

[<Literal>]
let private WorkerSectionName = "EmbassiesAppointmentsScheduler"

let getWorkerTasks () =
    async {
        let sectionResult =
            Configuration.getSection<Worker.Domain.Persistence.Task array> WorkerSectionName

        return
            match sectionResult with
            | None -> Error $"Section '%s{WorkerSectionName}' was not found."
            | Some tasks -> Ok <| Worker.Mapper.mapTasks tasks
    }

let getTaskSchedule name =
    async {
        let! tasksResult = getWorkerTasks ()

        return
            match tasksResult with
            | Error error -> Error error
            | Ok tasks ->

                let rec innerLoop targetName taskName (tasks: Worker.Domain.Core.Task list) =
                    tasks
                    |> List.map (fun task ->
                        let nodeName = taskName |> Infrastructure.DSL.Tree.buildNodeName <| task.Name

                        let result =
                            match nodeName = targetName with
                            | true -> Some task.Schedule
                            | _ ->
                                innerLoop targetName nodeName task.Steps
                                None

                        return result)

                match innerLoop name None tasks with
                | Some schedule -> Ok schedule
                | None -> Error $"Task '%s{name}'was not found in the section '%s{WorkerSectionName}'."
    }
