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

    let private getAppointments (credentials: Credentials) ct : Async<Result<Set<Appointment> option, AppError>> =
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

    let rec tryGetAppointments credentials attempts ct =
        async {
            match credentials with
            | [] -> return Ok None
            | head :: tail ->
                match! getAppointments head ct with
                | Ok None -> return Ok None
                | Ok(Some appointments) -> return Ok <| Some appointments
                | Error error ->
                    match error with
                    | Infrastructure(InvalidRequest _) ->
                        if attempts = 0 then
                            return Error error
                        else
                            return! tryGetAppointments tail (attempts - 1) ct
                    | _ -> return Error error
        }

    let findAppointments city ct attempts =
        async {
            match Repository.getMemoryStorage () with
            | Error error -> return Error <| Infrastructure error
            | Ok storage ->
                match! Repository.Russian.getCredentials city storage ct with
                | Error error -> return Error <| Infrastructure error
                | Ok None -> return Ok None
                | Ok(Some credentials) ->
                    let credentials = credentials |> List.ofSeq

                    match! tryGetAppointments credentials attempts ct with
                    | Error error -> return Error error
                    | Ok None -> return Ok None
                    | Ok(Some appointments) ->
                        match! Repository.Russian.setAppointments city appointments storage ct with
                        | Error error -> return Error <| Infrastructure error
                        | Ok _ -> return Ok <| Some appointments

        }

    let notifyUsers city ct =
        async {
            match Repository.getMemoryStorage () with
            | Error error -> return Error <| Infrastructure error
            | Ok storage ->
                match! Repository.Russian.getTelegramClients city storage ct with
                | Error error -> return Error <| Infrastructure error
                | Ok None -> return Ok None
                | Ok(Some clients) ->
                    match! Repository.Russian.getAppointments city storage ct with
                    | Error error -> return Error <| Infrastructure error
                    | Ok None -> return Ok None
                    | Ok(Some appointments) ->

                        let send client =
                            Web.Core.Bots.Telegram.sendMessage "token" "chatId" "message"

                        return response

        }
