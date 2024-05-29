module internal Eas.Worker.Configuration

open Infrastructure

let private file = Configuration.File.Yaml "appsettings"
let AppSettings = Configuration.get file

let getSection<'a> = AppSettings |> Configuration.getSection<'a>
