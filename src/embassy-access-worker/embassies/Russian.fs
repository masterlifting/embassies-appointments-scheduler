module internal EmbassyAccess.Worker.Embassies.Russian

open Infrastructure
open Infrastructure.Domain.Graph
open Persistence.Domain.Core
open Persistence.Storage.Core
open Worker.Domain.Internal
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Persistence
open EmbassyAccess.Persistence.Filter
open EmbassyAccess.Embassies.Russian
open EmbassyAccess.Embassies.Russian.Domain

let private createRequests ct country storage =
    let filter =
        { Pagination =
            { Page = 1
              PageSize = 5
              SortBy = Desc(Date(_.Modified)) }
          Embassy = Some <| Russian country
          Modified = None }

    storage |> Repository.Query.Request.get ct filter

let private tryGetAppointments ct storage requests =
    let deps = Api.createGetAppointmentsDeps ct storage
    let getAppointments = Api.getAppointments deps
    let deps = { getAppointments = getAppointments }
    requests |> Api.tryGetAppointments deps

let private handleAppointmentsResult ct storage response =

    let saveResponse response =
        storage |> Repository.Command.AppointmentsResponse.create ct response

    match response with
    | None -> async { return Ok <| Info "No appointments found." }
    | Some response ->
        response
        |> saveResponse
        |> ResultAsync.map (fun _ -> Success $"{response.Appointments.Count} appointments found.")

let private searchAppointments country =
    fun _ ct ->
        createStorage InMemory
        |> ResultAsync.wrap (fun storage ->
            storage
            |> createRequests ct country
            |> ResultAsync.bind' (tryGetAppointments ct storage)
            |> ResultAsync.bind' (handleAppointmentsResult ct storage))

let createNode country =
    Node(
        { Name = "Russian"; Handle = None },
        [ Node(
              { Name = "Search Appointments"
                Handle = Some <| searchAppointments country },
              []
          ) ]
    )
