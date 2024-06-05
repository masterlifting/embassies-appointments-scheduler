module internal Eas.Core

open System.Threading
open Eas.Domain.Internal
open Eas.Domain.Internal.Core
open Eas.Persistence
open Infrastructure.Domain.Errors

module Russian =
    open System
    open Domain.Internal.Core
    open Eas.Domain.Internal.Russian
    open Web.Core.Bots
    open Web.Domain.Internal.Bots.Telegram

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
                return Error <| Logical NotImplemented
            | Error error -> return Error <| Logical NotImplemented
        }

    let confirmKdmidOrder (credentials: Credentials) ct =
        async {
            let city, id, cd, ems = credentials.Value
            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Logical NotImplemented
        }

    // let rec private tryGetAppointments credentials attempts ct =
    //     async {
    //         match credentials with
    //         | [] -> return Ok None
    //         | head :: tail ->
    //             match! getAppointments' head ct with
    //             | Ok None -> return Ok None
    //             | Ok(Some appointments) -> return Ok <| Some appointments
    //             | Error error ->
    //                 match error with
    //                 | Infrastructure(InvalidRequest _) ->
    //                     if attempts = 0 then
    //                         return Error error
    //                     else
    //                         return! tryGetAppointments tail (attempts - 1) ct
    //                 | _ -> return Error error
    //     }

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
