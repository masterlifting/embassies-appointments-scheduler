module Configuration

open Infrastructure

let private appSettings = Configuration.getJsonConfiguration "appsettings.json"

let getSection<'a> = appSettings |> Configuration.getSection<'a>
