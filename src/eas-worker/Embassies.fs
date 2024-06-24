module internal Eas.Worker.Embassies

open Infrastructure.Dsl
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence.Filter
open Persistence.Domain

module Russian =

    let private getAvailableDates country =
        fun configuration ct ->
            Persistence.Core.createStorage InMemory
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
                    Eas.Core.Russian.getResponse configuration request ct

                let tryGetResponse requests =
                    Eas.Core.Russian.tryGetResponse requests updateRequest getResponse

                let saveResponse response =
                    Eas.Persistence.Repository.Command.Response.create storage response ct

                let handleResponse response =
                    match response with
                    | None -> async { return Ok <| Info "No data." }
                    | Some response ->
                        response
                        |> saveResponse
                        |> ResultAsync.map (fun _ -> Success response.Appointments)

                getRequests filter
                |> ResultAsync.bind' (tryGetResponse >> ResultAsync.bind' handleResponse))

    let createNode country =
        Node(
            { Name = "Russian"; Handle = None },
            [ Node(
                  { Name = "Look for appointments"
                    Handle = Some <| getAvailableDates country },
                  []
              ) ]
        )
