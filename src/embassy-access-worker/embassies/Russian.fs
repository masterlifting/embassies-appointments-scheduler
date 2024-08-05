module internal EmbassyAccess.Worker.Embassies.Russian

open Infrastructure
open Persistence.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Persistence

let private getRequests ct country storage =
    let filter: Filter.Request =
        { Pagination =
            Some
            <| { Page = 1
                 PageSize = 5
                 SortBy = Filter.Desc(Filter.Date(_.Modified)) }
          Ids = []
          Embassy = Some <| Russian country
          Modified = None }

    storage |> Repository.Query.Request.get ct filter

let private tryGetAppointments ct storage requests =
    (storage, ct)
    |> EmbassyAccess.Deps.Russian.getAppointments
    |> EmbassyAccess.Api.getAppointments
    |> EmbassyAccess.Api.tryGetAppointments requests

let private handleAppointmentsResponse ct storage (request: EmbassyAccess.Domain.Request option) =

    let updateRequest request =
        storage |> Repository.Command.Request.update ct request

    match request with
    | None -> async { return Ok <| Info "No appointments found." }
    | Some request -> 
        request 
        |> updateRequest
        |> ResultAsync.map(fun _ -> Success $"{request.Appointments.Count} appointments found.")

let private searchAppointments country =
    fun _ ct ->
        Persistence.Storage.create InMemory
        |> ResultAsync.wrap (fun storage ->
            storage
            |> getRequests ct country
            |> ResultAsync.bind' (tryGetAppointments ct storage)
            |> ResultAsync.bind' (handleAppointmentsResponse ct storage))

let createNode country =
    Graph.Node(
        { Name = "Russian"; Handle = None },
        [ Graph.Node(
              { Name = "Search Appointments"
                Handle = Some <| searchAppointments country },
              []
          ) ]
    )
