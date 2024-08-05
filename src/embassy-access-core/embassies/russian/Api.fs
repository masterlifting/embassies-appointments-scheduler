[<RequireQualifiedAccess>]
module internal EmbassyAccess.Embassies.Russian.Api

open Infrastructure
open EmbassyAccess.Domain
open EmbassyAccess.Embassies.Russian.Domain
open EmbassyAccess.Embassies.Russian.Core

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

    let createResult = ResultAsync.map' Helpers.createAppointmentsResult

    // pipe
    fun request -> request |> updateRequest |> createCredentials |> getAppointments |> createResult

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

    let createResult = ResultAsync.map' Helpers.createConfirmationResult

    // pipe
    fun option request ->
        (request, option)
        |> updateRequest
        |> createCredentials
        |> bookAppointment
        |> createResult
