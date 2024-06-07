module internal Eas.Core

open Eas.Domain.Internal.Core
open Eas.Persistence
open Infrastructure.DSL
open Infrastructure.Domain.Errors

module Russian =
    open Eas.Domain.Internal.Russian

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

    let private getAppointments (credentials: Credentials) ct : Async<Result<Set<Appointment>, ApiError>> =
        async {
            let city, id, cd, ems = credentials.Value

            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems

            match Web.Core.Http.Mapper.toUri <| baseUrl + urlParams with
            | Ok uri ->
                let! response = getCalendarPage uri
                return Error <| Logical(NotImplemented "getAppointments")
            | Error error -> return Error <| (Logical <| NotImplemented "getAppointments")
        }

    let confirmKdmidOrder (credentials: Credentials) ct =
        async {
            let city, id, cd, ems = credentials.Value
            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Logical(NotImplemented "confirmKdmidOrder")
        }

    let getUserCredentials storage user country ct =
        Repository.Russian.initGetUserCredentials storage user country ct
        |> ResultAsync.mapError Infrastructure

    let getCountryCredentials storage country ct =
        Repository.Russian.initGetCountryCredentials storage country ct
        |> ResultAsync.mapError Infrastructure

    let setCredentials storage user country credentials ct =
        Repository.Russian.initSetCredentials storage user country credentials ct
        |> ResultAsync.mapError Infrastructure

    let getEmbassyResponse (request: Request) storage ct =
        async {
            match createCredentials request.Data with
            | Error error -> return Error <| Infrastructure error
            | Ok credentials ->
                match! getAppointments credentials ct with
                | Error error -> return Error error
                | Ok appointments ->
                    match appointments with
                    | appointments when appointments.Count = 0 -> return Ok None
                    | appointments ->
                        return
                            Ok
                            <| Some
                                { Embassy = request.Embassy
                                  Appointments = appointments
                                  Data = Map [ "credentials", request.Data ] }
        }

    let setEmbassyResponse (response: Response) storage ct =
        async {
            match response.Data |> Map.tryFind "credentials" with
            | None -> return Error <| (Infrastructure <| InvalidRequest "No credentials found in response.")
            | Some credentials ->
                match createCredentials credentials with
                | Error error -> return Error <| Infrastructure error
                | Ok credentials ->
                    match! confirmKdmidOrder credentials ct with
                    | Error error -> return Error error
                    | Ok _ -> return Ok $"Credentials for {credentials.City} are set."
        }
