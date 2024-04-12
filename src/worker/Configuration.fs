module Configuration

open Infrastructure

let private appSettings = Configuration.setJsonConfiguration "appsettings.json"

let getSection<'a> = appSettings |> Configuration.getSection<'a>
