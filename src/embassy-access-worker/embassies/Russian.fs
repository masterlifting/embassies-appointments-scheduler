module internal EmbassyAccess.Worker.Embassies.Russian

open Infrastructure
open Persistence.Domain
open Worker.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Persistence

module private SearchAppointments =

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
        let getAppointments =
            (storage, ct)
            |> EmbassyAccess.Deps.Russian.getAppointments
            |> EmbassyAccess.Api.getAppointments

        let rec innerLoop (requests: Request list) (errors: Error' list) =
            async {
                match requests with
                | [] ->
                    return
                        match errors.Length with
                        | 0 -> Ok None
                        | _ ->
                            let msg =
                                errors
                                |> List.mapi (fun i error -> $"{i + 1}.{error.Message}")
                                |> String.concat "\n"

                            let error =
                                Operation
                                    { Message = $"Multiple errors: \n{msg}"
                                      Code = None }

                            Error error
                | request :: requestsTail ->
                    match! getAppointments request with
                    | Error error' ->
                        let errors = errors @ [ error' ]
                        return! innerLoop requestsTail errors
                    | Ok appointments ->
                        let request =
                            { request with
                                Appointments = appointments }

                        Some request
            }

        let a =innerLoop requests []

    let private handleAppointmentsResponse ct storage (request: EmbassyAccess.Domain.Request) =

        let request =
            { request with
                Modified = System.DateTime.UtcNow }

        let updateRequest request =
            storage |> Repository.Command.Request.update ct request

        match request.Appointments.IsEmpty with
        | true -> async { return Ok <| Info "No appointments found." }
        | false ->
            request
            |> updateRequest
            |> ResultAsync.map (fun _ -> Success $"{request.Appointments.Count} appointments found.")

    let run country =
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
                Handle = Some <| SearchAppointments.run country },
              []
          ) ]
    )
