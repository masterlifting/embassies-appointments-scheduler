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

        let rec innerLoop requests (errors: Error' list) =
            async {
                match requests with
                | [] ->
                    return
                        match errors.IsEmpty with
                        | true -> Ok None
                        | false ->
                            let msg =
                                errors
                                |> List.mapi (fun i error -> $"{i + 1}.{error.Message}")
                                |> String.concat "\n"

                            Error
                            <| Operation
                                { Message = $"Multiple errors: \n{msg}"
                                  Code = None }

                | request :: requestsTail ->
                    match! getAppointments request with
                    | Error error' -> return! innerLoop requestsTail (errors @ [ error' ])
                    | Ok request -> return Ok <| Some request
            }

        innerLoop requests []

    let private handleAppointmentsResponse request =
        match request with
        | None -> Ok <| Info "No appointments found."
        | Some request ->
            match request.State with
            | Failed ->
                let error = request.Description |> Option.defaultValue "Unknown error."
                Error <| Operation { Message = error; Code = None }
            | _ ->
                match request.Appointments.IsEmpty with
                | true -> Ok <| Info "No appointments found."
                | false -> Ok <| Info $"Found {request.Appointments.Count} appointments."

    let run country =
        fun _ ct ->
            Persistence.Storage.create InMemory
            |> ResultAsync.wrap (fun storage ->
                storage
                |> getRequests ct country
                |> ResultAsync.bind' (tryGetAppointments ct storage)
                |> ResultAsync.bind handleAppointmentsResponse)

let createNode country =
    Graph.Node(
        { Name = "Russian"; Handle = None },
        [ Graph.Node(
              { Name = "Search Appointments"
                Handle = Some <| SearchAppointments.run country },
              []
          ) ]
    )
