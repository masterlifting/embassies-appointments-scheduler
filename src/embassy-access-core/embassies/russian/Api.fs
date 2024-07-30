[<RequireQualifiedAccess>]
module internal EmbassyAccess.Embassies.Russian.Api

open Infrastructure
open EmbassyAccess.Domain.Internal
open EmbassyAccess.Embassies.Russian.Domain
open EmbassyAccess.Embassies.Russian.Core

let createGetAppointmentsDeps = Deps.createGetAppointmentsDeps

let createBookAppointmentDeps = Deps.createBookAppointmentDeps

let createTryGetAppointmentsDeps = Deps.createTryGetAppointmentsDeps

let getAppointments deps =

    // define
    let inline updateRequest request =
        request
        |> Helpers.updateRequest
        |> ResultAsync.wrap deps.updateRequest
        |> ResultAsync.map (fun _ -> request)

    let createCredentials =
        ResultAsync.bind (fun request ->
            createCredentials request.Value
            |> Result.bind (fun credentials ->
                (request, credentials)
                |> Helpers.checkCredentials
                |> Result.map (fun _ -> request, credentials)))

    let getAppointments =
        ResultAsync.bind' (fun (request, credentials) ->
            getAppointments deps credentials
            |> ResultAsync.map (fun (appointments, _) -> request, appointments))

    let createResult =
        ResultAsync.map' (fun (request, appointments: Set<Appointment>) ->
            match appointments.IsEmpty with
            | true -> None
            | false -> Some <| (request |> Helpers.createAppointmentsResponse appointments))

    // pipe
    fun request ->
        request
        |> updateRequest
        |> createCredentials
        |> getAppointments
        |> createResult

let bookAppointment deps =

    // define
    let inline updateRequest (request, option) =
        request
        |> Helpers.updateRequest
        |> ResultAsync.wrap deps.GetAppointmentsDeps.updateRequest
        |> ResultAsync.map (fun _ -> request, option)

    let createCredentials =
        ResultAsync.bind (fun (request, option) ->
            createCredentials request.Value
            |> Result.bind (fun credentials ->
                (request, credentials)
                |> Helpers.checkCredentials
                |> Result.map (fun _ -> request, option, credentials)))

    let bookAppointment =
        ResultAsync.bind' (fun (request, option, credentials) ->
            bookAppointment deps option credentials
            |> ResultAsync.map (fun result -> request, result))

    let createResult =
        ResultAsync.map' (fun (request, result) -> request |> Helpers.createConfirmationResponse result)

    // pipe
    fun request option ->
        (request, option)
        |> updateRequest
        |> createCredentials
        |> bookAppointment
        |> createResult

let tryGetAppointments deps =

    let rec innerLoop requests error =
        async {
            match requests with
            | [] ->
                return
                    match error with
                    | Some error -> Error error
                    | None -> Ok None
            | request :: requestsTail ->
                match! deps.getAppointments request with
                | Error error -> return! innerLoop requestsTail (Some error)
                | response -> return response
        }

    fun requests -> innerLoop requests None
