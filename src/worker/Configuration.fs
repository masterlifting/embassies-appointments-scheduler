module Configuration

open Infrastructure

let private configuration = Configuration.setJsonConfiguration "appsettings.json"

let getSection<'a> = configuration |> Configuration.getSection<'a>
