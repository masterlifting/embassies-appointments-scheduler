module Eas.Core

open System.Threading
open Eas.Domain.Core
open Eas.Persistence
open Infrastructure.Domain.Errors

module Russian =
    open System
    open Eas.Domain.Core.Russian

    let private createBaseUrl city = $"https://kdmid.ru/queue/%s{city}/"

    let private createUrlParams id cd ems =
        match ems with
        | Some ems -> $"?id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"?id=%i{id}&cd=%s{cd}"

    let private getStartPage () =
        async {
            match Web.Core.Http.Mapper.toUri "https://kdmid.ru/" with
            | Ok uri ->
                let! response = Web.Core.Http.get uri
                return response
            | Error error -> return Error error
        }

    let private getCapchaImage () =
        async {
            match Web.Core.Http.Mapper.toUri "https://kdmid.ru/captcha/" with
            | Ok uri ->
                let! response = Web.Core.Http.get uri
                return response
            | Error error -> return Error error
        }

    let private solveCapcha (image: byte[]) =
        async {
            match Web.Core.Http.Mapper.toUri "https://kdmid.ru/captcha/" with
            | Ok uri ->
                let! response = Web.Core.Http.get uri
                return response
            | Error error -> return Error error
        }

    let private postStartPage (data: string) =
        async { return Error "postStartPage not implemented." }

    let private getCalendarPage uri =
        async {
            let! response = Web.Core.Http.get uri
            return response
        }

    let private getAppointments (credentials: Credentials) : Async<Result<Response, AppError>> =
        async {
            let city, id, cd, ems = credentials.Value

            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems

            match Web.Core.Http.Mapper.toUri <| baseUrl + urlParams with
            | Ok uri ->
                let! response = getCalendarPage uri
                return Error <| Logical NotImplemented
            | Error error -> return Error <| Logical NotImplemented
        }

    let confirmKdmidOrder (credentials: Credentials) =
        async {
            let city, id, cd, ems = credentials.Value
            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Logical NotImplemented
        }

    let getAvailableDates (city: City) (ct: CancellationToken) =
        async {
            let! credentialsSetRes = Repository.Russian.getCredentials city ct

            match credentialsSetRes with
            | Error error -> return Error <| Infrastructure error
            | Ok None -> return Ok None
            | Ok(Some credentialsSet) ->
                match! credentialsSet |> Seq.head |> getAppointments with
                | Error error -> return Error error
                | Ok response -> return Ok <| Some response

        }

    let notifyUsers (city: City) (ct: CancellationToken) = async { return Ok <| None }
