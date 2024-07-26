module internal EmbassyAccess.Worker.Embassies

open Infrastructure
open Infrastructure.Domain.Graph
open Persistence.Domain.Core
open Persistence.Storage.Core
open Worker.Domain.Internal
open EmbassyAccess.Core
open EmbassyAccess.Domain.Core.Internal
open EmbassyAccess.Persistence.Core
open EmbassyAccess.Persistence.Core.Filter

module Russian =
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
        let deps = Russian.API.createGetAppointmentsDeps ct storage
        let getAppointments = Russian.API.getAppointments deps
        requests |> Russian.API.tryGetAppointments getAppointments

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
