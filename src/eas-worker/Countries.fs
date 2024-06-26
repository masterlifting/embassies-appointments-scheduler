﻿module internal Eas.Worker.Countries

open Infrastructure.Domain.Graph
open Infrastructure.Dsl
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence
open Persistence.Domain

module Serbia =
    let private createTestRequest =
        fun _ ct ->
            Persistence.Core.createStorage InMemory
            |> ResultAsync.wrap (fun storage ->
                let request =
                    { Id = System.Guid.NewGuid() |> RequestId
                      User = { Id = UserId 1; Name = "Andrei" }
                      Embassy = Russian <| Serbia Belgrade
                      Data =
                        Map
                            [ "url", "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                      Modified = System.DateTime.UtcNow }

                storage
                |> Repository.Command.Request.create ct request
                |> ResultAsync.map (fun _ -> Info $"Test request was created \n{request}"))

    let private Belgrade =
        Node(
            { Name = "Belgrade"
              Handle = Some <| createTestRequest },
            [ Embassies.Russian.createNode <| Serbia Belgrade ]
        )

    let Node = Node({ Name = "Serbia"; Handle = None }, [ Belgrade ])

module Bosnia =
    let private Sarajevo =
        Node({ Name = "Sarajevo"; Handle = None }, [ Embassies.Russian.createNode <| Bosnia Sarajevo ])

    let Node = Node({ Name = "Bosnia"; Handle = None }, [ Sarajevo ])

module Montenegro =
    let private Podgorica =
        Node({ Name = "Podgorica"; Handle = None }, [ Embassies.Russian.createNode <| Montenegro Podgorica ])

    let Node = Node({ Name = "Montenegro"; Handle = None }, [ Podgorica ])

module Albania =
    let private Tirana =
        Node({ Name = "Tirana"; Handle = None }, [ Embassies.Russian.createNode <| Albania Tirana ])

    let Node = Node({ Name = "Albania"; Handle = None }, [ Tirana ])

module Hungary =
    let private Budapest =
        Node({ Name = "Budapest"; Handle = None }, [ Embassies.Russian.createNode <| Hungary Budapest ])

    let Node = Node({ Name = "Hungary"; Handle = None }, [ Budapest ])
