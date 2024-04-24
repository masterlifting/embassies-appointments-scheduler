module KdmidScheduler.Worker.Configuration

open Infrastructure

let internal AppSettings = Configuration.getJsonConfiguration "appsettings.json"
let getSection<'a> = AppSettings |> Configuration.getSection<'a>
