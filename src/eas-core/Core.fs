module internal Eas.Core

open System
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =

    let private createBaseUrl city = $"https://kdmid.ru/queue/%s{city}/"

    let private createUrlParams id cd ems =
        match ems with
        | Some ems -> $"?id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"?id=%i{id}&cd=%s{cd}"

    let private getStartPage () = Web.Core.Http.get "https://kdmid.ru/"

    let private getCapchaImage () =
        Web.Core.Http.get "https://kdmid.ru/captcha/"

    let private solveCapcha (image: byte[]) =
        Web.Core.Http.post "https://kdmid.ru/captcha/" image

    let private postStartPage (data: string) =
        async { return Error "postStartPage not implemented." }

    let private getCalendarPage url =
        async {
            let response = Web.Core.Http.get url
            return response
        }

    let private getAppointments
        (credentials: Embassies.Russian.Credentials)
        ct
        : Async<Result<Set<Appointment>, ApiError>> =
        async {
            let city, id, cd, ems = credentials.Value

            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems

            let! response = getCalendarPage (baseUrl + urlParams)

            return Error <| (Logical <| NotImplemented "getAppointments")
        }

    let confirmKdmidOrder (credentials: Embassies.Russian.Credentials) ct =
        async {
            let city, id, cd, ems = credentials.Value
            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Logical(NotImplemented "confirmKdmidOrder")
        }

    let initGetEmbassyResponse storage =
        fun (request: Request) ct ->
            async {
                match request.Data |> Map.tryFind "url" with
                | None -> return Error <| (Infrastructure <| InvalidRequest "No url found in request.")
                | Some url ->
                    match Embassies.Russian.createCredentials url with
                    | Error error -> return Error <| Infrastructure error
                    | Ok credentials ->
                        match! getAppointments credentials ct with
                        | Error error -> 
                            match error with
                            | Infrastructure(InvalidRequest _)
                            | Infrastructure(InvalidResponse _) -> 

                            

                                return Ok None
                            | _ -> return Error error
                        | Ok appointments ->
                            match appointments with
                            | appointments when appointments.Count = 0 -> return Ok None
                            | appointments ->
                                return
                                    Ok
                                    <| Some
                                        { Id = Guid.NewGuid() |> ResponseId
                                          Embassy = request.Embassy
                                          Appointments = appointments
                                          Data = request.Data
                                          Modified = DateTime.UtcNow }
            }

    let setEmbassyResponse (response: Response) storage ct =
        async {
            match response.Data |> Map.tryFind "credentials" with
            | None -> return Error <| (Infrastructure <| InvalidRequest "No credentials found in response.")
            | Some credentials ->
                match Embassies.Russian.createCredentials credentials with
                | Error error -> return Error <| Infrastructure error
                | Ok credentials ->
                    match! confirmKdmidOrder credentials ct with
                    | Error error -> return Error error
                    | Ok _ -> return Ok $"Credentials for {credentials.City} are set."
        }
