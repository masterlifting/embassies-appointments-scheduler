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
            |> Result.bind (fun credentials -> (request, credentials) |> Helpers.checkCredentials |> Result.map snd))

    let getAppointments =
        ResultAsync.bind' (getAppointments deps >> ResultAsync.map fst)

    // pipe
    fun request -> request |> updateRequest |> createCredentials |> getAppointments

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
                |> Result.map (fun _ -> option, credentials)))

    let bookAppointment = ResultAsync.bind' (bookAppointment deps)

    // pipe
    fun option request -> (request, option) |> updateRequest |> createCredentials |> bookAppointment
