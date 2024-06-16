module internal Eas.Worker.Embassies

open Infrastructure.Dsl
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Persistence.Domain
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence.Filter

module Russian =
    let private getAvailableDates country =
        fun ct ->
            Persistence.Core.createStorage InMemory
            |> Result.mapError Infrastructure
            |> ResultAsync.wrap (fun storage ->

                let requestsFilter =
                    Request.ByEmbassy(
                        { Pagination =
                            { Page = 1
                              PageSize = 5
                              SortBy = Desc(Date(fun x -> x.Modified)) }
                          Embassy = Russian country }
                    )

                let getRequests filter =
                    Eas.Persistence.Repository.Query.Request.get storage filter ct

                let getResponse request ct =
                    Eas.Core.Russian.getResponse storage request ct

                let tryGetResponse requests =
                    Eas.Core.Russian.tryGetResponse requests ct getResponse

                let setResponse response =
                    Eas.Core.Russian.setResponse storage response ct

                async {

                    //TODO: remove this block after the development
                    let createRequest request =
                        Eas.Persistence.Repository.Command.Request.create storage request ct

                    let testRequest =
                        { Id = System.Guid.NewGuid() |> RequestId
                          User = { Id = UserId 0; Name = "Andrei" }
                          Embassy = Russian country
                          Data =
                            Map
                                [ "url",
                                  "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                          Modified = System.DateTime.UtcNow }

                    let! _ = createRequest testRequest
                    //

                    match! getRequests requestsFilter with
                    | Error error -> return Error error
                    | Ok requests ->
                        match! tryGetResponse requests with
                        | Error error -> return Error error
                        | Ok None -> return Ok <| Info "No data."
                        | Ok(Some response) ->
                            match! setResponse response with
                            | Error error -> return Error error
                            | _ -> return Ok <| Success response.Appointments
                })

    let createNode country =
        Node(
            { Name = "RussianEmbassy"
              Handle = None },
            [ Node(
                  { Name = "GetAvailableDates"
                    Handle = Some <| getAvailableDates country },
                  []
              ) ]
        )
