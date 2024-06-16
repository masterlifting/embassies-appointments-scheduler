module Eas.Core

open System
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Embassies.Russian

    let private createBaseUrl city = $"https://%s{city}.kdmid.ru/queue/"

    let private createUrlParams id cd ems =
        match ems with
        | Some ems -> $"?id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"?id=%i{id}&cd=%s{cd}"

    let private getStartPage baseUrl urlParams : string =
        let requestUrl = baseUrl + "OrderInfo.aspx" + urlParams
        Web.Core.Http.get requestUrl

    let private getCapchaImage () =
        Web.Core.Http.get "https://kdmid.ru/captcha/"

    let private solveCapcha (image: byte[]) =
        Web.Core.Http.post "https://kdmid.ru/captcha/" image

    let private postStartPage (data: string) =
        async { return Error "postStartPage not implemented." }

    let private getCalendarPage baseUrl urlParams =
        //getStartPage baseUrl urlParams
        async { return Error(Logical(NotImplemented "getCalendarPage")) }

    let private getAppointments (credentials: Credentials) ct : Async<Result<Set<Appointment>, ApiError>> =
        let city, id, cd, ems = credentials.Value
        let baseUrl = createBaseUrl city
        let urlParams = createUrlParams id cd ems

        async {
            let! startPage = getStartPage baseUrl urlParams
            return Error(Logical(NotImplemented "getAppointments"))
        }

    let confirmKdmidOrder (credentials: Credentials) ct =
        async {
            let city, id, cd, ems = credentials.Value
            let baseUrl = createBaseUrl city
            let urlParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Logical(NotImplemented "confirmKdmidOrder")
        }

    let getResponse storage (request: Request) ct =

        let updateRequest request =
            Persistence.Repository.Command.Request.update storage request ct

        async {
            match request.Data |> Map.tryFind "url" with
            | None -> return Error(Infrastructure(InvalidRequest "No url found in requests data."))
            | Some requestUrl ->
                match createCredentials requestUrl with
                | Error error -> return Error(Infrastructure error)
                | Ok credentials ->
                    match! getAppointments credentials ct with
                    | Error(Infrastructure(InvalidRequest msg))
                    | Error(Infrastructure(InvalidResponse msg)) ->

                        let! updateRes =
                            updateRequest
                                { request with
                                    Modified = DateTime.UtcNow }

                        match updateRes with
                        | Ok _ -> return Error(Infrastructure(InvalidRequest msg))
                        | Error error -> return Error error

                    | Error error -> return Error error
                    | Ok appointments ->
                        match appointments with
                        | appointments when appointments.Count = 0 -> return Ok None
                        | appointments ->
                            return
                                Ok
                                <| Some
                                    { Id = Guid.NewGuid() |> ResponseId
                                      Request = request
                                      Appointments = appointments
                                      Data = request.Data
                                      Modified = DateTime.UtcNow }
        }

    let tryGetResponse requests ct getResponse =

        let rec innerLoop requests error =
            async {
                match requests with
                | [] ->
                    return
                        match error with
                        | Some error -> Error error
                        | None -> Ok None
                | request :: requestsTail ->
                    match! getResponse request ct with
                    | Error error -> return! innerLoop requestsTail (Some error)
                    | response -> return response
            }

        innerLoop requests None

    let setResponse storage (response: Response) ct =
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
