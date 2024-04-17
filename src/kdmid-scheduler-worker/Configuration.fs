module KdmidScheduler.Worker.Configuration

open Infrastructure

let internal appSettings = Configuration.getJsonConfiguration "appsettings.json"

let getSection<'a> = appSettings |> Configuration.getSection<'a>
