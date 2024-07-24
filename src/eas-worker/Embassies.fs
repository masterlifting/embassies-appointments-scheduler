module internal Eas.Worker.Embassies

open Infrastructure.DSL
open Infrastructure.Domain.Graph
open Worker.Domain.Internal
open Eas.Core
open Eas.Domain.Internal
open Eas.Persistence.Filter

module Russian =
    open Persistence.Domain.Core
    open Persistence.Storage.Core
    open Eas.Persistence

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

        let deps = Russian.API.createGetAppointmentsDeps ct

        let getAppointments = Russian.API.getAppointments deps

        let updateRequest request =
            storage |> Repository.Command.Request.update ct request

        requests
        |> Russian.API.tryGetAppointments
            { updateRequest = updateRequest
              getAppointments = getAppointments }

    let private handleAppointmentsResult ct storage response =

        let saveResponse response =
            storage |> Repository.Command.AppointmentsResponse.create ct response

        match response with
        | None -> async { return Ok <| Info "No appointments found." }
        | Some response ->
            response
            |> saveResponse
            |> ResultAsync.map (fun _ -> Success $"\n{response.Appointments}")

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
