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

let private getAppointmentsResponses ct storage (response: AppointmentsResponse) =
    let filter: Filter.AppointmentsResponse =
        { Pagination = None
          Ids = []
          Request = response.Request |> Some
          Modified = None }

    storage |> Repository.Query.AppointmentsResponse.get ct filter

let private updateAppointmentsResponse ct response storage =
    storage |> Repository.Command.AppointmentsResponse.update ct response

let private createAppointmentsResponse ct response storage =
    storage |> Repository.Command.AppointmentsResponse.create ct response

let private handleAppointmentsResponse ct storage response =

    let saveAppointmentsResponse ct response (responses: AppointmentsResponse list) =
        match responses.Length with
        | 0 -> storage |> createAppointmentsResponse ct response
        | 1 ->
            let oldResponse = responses[0]

            let newResponse =
                { oldResponse with
                    Appointments = response.Appointments
                    Modified = System.DateTime.UtcNow }

            storage |> updateAppointmentsResponse ct newResponse
        | _ ->
            async {
                return
                    Error
                    <| Operation
                        { Message = "Multiple appointments responses found."
                          Code = None }
            }


    match response with
    | None -> async { return Ok <| Info "No appointments found." }
    | Some response ->
        response
        |> getAppointmentsResponses ct storage
        |> ResultAsync.bind' (saveAppointmentsResponse ct response)
        |> ResultAsync.map (fun _ -> Success $"{response.Appointments.Count} appointments found.")

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
