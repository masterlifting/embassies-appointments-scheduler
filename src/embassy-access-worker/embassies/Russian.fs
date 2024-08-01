module internal EmbassyAccess.Worker.Embassies.Russian

open Infrastructure
open Persistence.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Persistence
open EmbassyAccess.Embassies.Russian

let private createRequests ct country storage =
    let filter: Filter.Request =
        { Pagination =
            { Page = 1
              PageSize = 5
              SortBy = Filter.Desc(Filter.Date(_.Modified)) }
          Embassy = Some <| Russian country
          Modified = None }

    storage |> Repository.Query.Request.get ct filter

let private tryGetAppointments ct storage requests =

    let deps =
        Deps.createGetAppointmentsDeps ct storage
        |> EmbassyAccess.Api.GetAppointmentsDeps.Russian

    let getAppointments = EmbassyAccess.Api.getAppointments deps
    requests |> EmbassyAccess.Api.tryGetAppointments getAppointments

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
        Persistence.Storage.create InMemory
        |> ResultAsync.wrap (fun storage ->
            storage
            |> createRequests ct country
            |> ResultAsync.bind' (tryGetAppointments ct storage)
            |> ResultAsync.bind' (handleAppointmentsResult ct storage))

let createNode country =
    Graph.Node(
        { Name = "Russian"; Handle = None },
        [ Graph.Node(
              { Name = "Search Appointments"
                Handle = Some <| searchAppointments country },
              []
          ) ]
    )
