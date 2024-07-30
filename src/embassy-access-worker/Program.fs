open Infrastructure
open Infrastructure.Domain.Graph
open Persistence.Storage.Core
open Persistence.Domain.Core
open Worker.Domain.Internal
open EmbassyAccess.Worker
open EmbassyAccess.Worker.Countries
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Persistence

let private createRussianTestRequest ct (value, country) =
    createStorage InMemory
    |> ResultAsync.wrap (fun storage ->
        let request =
            { Id = System.Guid.NewGuid() |> RequestId
              Value = value
              Attempt = 0
              Embassy = Russian <| country
              Modified = System.DateTime.UtcNow }

        storage
        |> Repository.Command.Request.create ct request
        |> ResultAsync.map (fun _ -> Success "Test request was created."))

[<EntryPoint>]
let main _ =

    let configuration = Configuration.getYaml "appsettings"
    Logging.useConsole configuration

    let rootNode =
        { Name = "Scheduler"
          Handle =
            Some
            <| fun _ ct ->
                async {
                    let! testRequests =
                        [ ("https://berlin.kdmid.ru/queue/orderinfo.aspx?id=290383&cd=B714253F", Germany Berlin)
                          ("https://belgrad.kdmid.ru/queue/orderinfo.aspx?id=72096&cd=7FE4D97C&ems=7EE040C9",
                           Serbia Belgrade)
                          ("https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20779&cd=99CEBA38", Bosnia Sarajevo)
                          ("https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20780&cd=4FC17A57", Bosnia Sarajevo)
                          ("https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=F23CB539", Bosnia Sarajevo)
                          ("https://podgorica.kdmid.ru/queue/orderinfo.aspx?id=57123&cd=c73761c3&ems=09C3476F",
                           Montenegro Podgorica)
                          ("https://dublin.kdmid.ru/queue/orderinfo.aspx?id=22609&cd=2831b69e&ems=C7E84DE3",
                           Ireland Dublin)
                          ("https://bern.kdmid.ru/queue/orderinfo.aspx?id=42175&cd=8a623fd6&ems=7EFC4F7B",
                           Switzerland Bern)
                          ("https://helsinki.kdmid.ru/queue/orderinfo.aspx?id=202338&cd=b67019fa&ems=4B73480A",
                           Finland Helsinki)
                          ("https://paris.kdmid.ru/queue/orderinfo.aspx?id=368151&cd=775a8471&ems=6EFE4EFD",
                           France Paris)
                          ("https://hague.kdmid.ru/queue/orderinfo.aspx?id=114878&cd=f1e14d11&ems=2CAA46D6",
                           Netherlands Hague)
                          ("https://tirana.kdmid.ru/queue/orderinfo.aspx?id=7316&cd=548bbda9&ems=2F5343DA",
                           Albania Tirana)
                          ("https://ljubljana.kdmid.ru/queue/orderinfo.aspx?id=22474&cd=03dab4d2&ems=6CCA463E",
                           Slovenia Ljubljana) ]
                        |> List.map (createRussianTestRequest ct)
                        |> Async.Sequential

                    return
                        testRequests
                        |> Seq.roe
                        |> Result.map (fun _ -> Success "Test requests were created. Sceduler has started...")
                } }

    let handlersGraph =
        Node(
            rootNode,
            [ Albania.Node
              Bosnia.Node
              Finland.Node
              France.Node
              Germany.Node
              Hungary.Node
              Ireland.Node
              Montenegro.Node
              Netherlands.Node
              Serbia.Node
              Slovenia.Node
              Switzerland.Node ]
        )

    "Scheduler"
    |> Worker.Core.start
        { getTask = TasksStorage.getTask handlersGraph configuration
          Configuration = configuration }
    |> Async.RunSynchronously

    0
