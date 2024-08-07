[<RequireQualifiedAccess>]
module internal EmbassyAccess.Embassies.Russian.Api

open Infrastructure
open EmbassyAccess.Domain
open EmbassyAccess.Embassies.Russian.Domain
open EmbassyAccess.Embassies.Russian.Core

let getAppointments deps =

    // define
    let inline prepareRequest request =
        request
        |> Helpers.prepareRequest
        |> ResultAsync.wrap deps.updateRequest
        |> ResultAsync.map (fun _ -> request)

    let createCredentials =
        ResultAsync.bind (fun request ->
            createCredentials request.Value
            |> Result.bind (fun credentials -> (request, credentials) |> Helpers.validateCredentials))

    let getAppointments =
        ResultAsync.bind' (fun (request, credentials) ->
            credentials
            |> getAppointments deps
            |> ResultAsync.map (fun (appointments, _) -> (request, appointments)))

    let completeRequest =
        ResultAsync.bind' (fun (request, appointments) ->
            request
            |> Helpers.completeRequest
            |> deps.updateRequest
            |> ResultAsync.map (fun _ -> appointments))
        
    let completeRequestError request=
        ResultAsync.mapError' (fun error ->
            request
            |> Helpers.failedRequest
            |> deps.updateRequest
            |> ResultAsync.bind (fun _ -> Error error))

    // pipe
    fun request ->
        request
        |> prepareRequest
        |> createCredentials
        |> getAppointments
        |> completeRequest
        |> completeRequestError request

let bookAppointment deps =

    // define
    let inline prepareRequest (request, option) =
        request
        |> Helpers.prepareRequest
        |> ResultAsync.wrap deps.GetAppointmentsDeps.updateRequest
        |> ResultAsync.map (fun _ -> request, option)

    let createCredentials =
        ResultAsync.bind (fun (request, option) ->
            createCredentials request.Value
            |> Result.bind (fun credentials ->
                (request, credentials)
                |> Helpers.validateCredentials
                |> Result.map (fun _ -> request, option, credentials)))

    let bookAppointment =
        ResultAsync.bind' (fun (request, option, credentials) ->
            (option, credentials)
            |> bookAppointment deps
            |> ResultAsync.map (fun appointment -> (request, appointment)))

    let completeRequest =
        ResultAsync.bind' (fun (request, appointment) ->
            request
            |> Helpers.completeRequest
            |> deps.GetAppointmentsDeps.updateRequest
            |> ResultAsync.map (fun _ -> appointment))
        
    let completeRequestError request=
         ResultAsync.mapError' (fun error ->
            request
            |> Helpers.failedRequest
            |> deps.GetAppointmentsDeps.updateRequest
            |> ResultAsync.map (fun _ -> error))
        
    // pipe
    fun option request ->
        (request, option)
        |> prepareRequest
        |> createCredentials
        |> bookAppointment
        |> completeRequest
        |> completeRequestError request
