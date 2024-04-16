module Configuration

let internal appSettings =
    Infrastructure.Configuration.getJsonConfiguration "appsettings.json"

let getSection<'a> = appSettings |> Infrastructure.Configuration.getSection<'a>
