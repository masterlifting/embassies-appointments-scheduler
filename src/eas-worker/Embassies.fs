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

                let filter =
                    Request.ByEmbassy(
                        { Pagination =
                            { Page = 1
                              PageSize = 5
                              SortBy = Desc(Date(fun x -> x.Modified)) }
                          Embassy = Russian country }
                    )

                let getRequests filter =
                    Eas.Persistence.Repository.Query.Request.get storage filter ct

                let updateRequest request =
                    Eas.Persistence.Repository.Command.Request.update storage request ct

                let getResponse request =
                    Eas.Core.Russian.getResponse storage request ct

                let tryGetResponse requests =
                    Eas.Core.Russian.tryGetResponse requests updateRequest getResponse

                let setResponse response =
                    Eas.Core.Russian.setResponse storage response ct

                getRequests filter
                |> ResultAsync.bind' (
                    tryGetResponse
                    >> ResultAsync.bind' (fun response ->
                        match response with
                        | None -> async { return Ok <| Info "No data." }
                        | Some response ->
                            setResponse response |> ResultAsync.map (fun _ -> Success response.Appointments))
                ))

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
