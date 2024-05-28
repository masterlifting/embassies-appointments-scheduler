module internal Eas.Worker.Configuration

open Infrastructure

let AppSettings = Configuration.getYamlConfiguration "appsettings.yaml"
let getSection<'a> = AppSettings |> Configuration.getSection<'a>
