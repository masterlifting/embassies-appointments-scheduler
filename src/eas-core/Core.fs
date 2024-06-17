module Eas.Core

open System
open Infrastructure.Dsl
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Embassies.Russian
    open Web.Domain

    let private createBaseUrl city = $"https://%s{city}.kdmid.ru"

    let private createQueryParams id cd ems =
        match ems with
        | Some ems -> $"?id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"?id=%i{id}&cd=%s{cd}"

    let private getStartPage client queryParams : Async<Result<(Map<string, string> * string), ApiError>> =
        let requestUrl = "/queue/orderisnfo.aspx" + queryParams
        //Web.Http.get client requestUrl
        async { return Error(Logical(NotImplemented "getStartPage")) }

    let private getCaptcha (code: string) queryParams : Async<Result<byte array, ApiError>> =
        async { return Error(Logical(NotImplemented "getCapcha")) }

    let private solveCaptcha (image: byte[]) : Async<Result<string, ApiError>> =
        async { return Error(Logical(NotImplemented "solveCaptcha")) }

    let postStartPage
        (data: Map<string, string>)
        (captcha: string)
        queryParams
        : Async<Result<Map<string, string>, ApiError>> =
        async { return Error(Logical(NotImplemented "postStartPage")) }

    let private postCalendarPage
        (data: Map<string, string>)
        queryParams
        : Async<Result<(Map<string, string> * Set<Appointment>), ApiError>> =
        //getStartPage baseUrl urlParams
        async { return Error(Logical(NotImplemented "getCalendarPage")) }

    let private postConfirmation (data: Map<string, string>) apointment queryParams : Async<Result<unit, ApiError>> =
        async { return Error(Logical(NotImplemented "postConfirmation")) }


    let private getKdmidResponse (credentials: Credentials) ct : Async<Result<Response, ApiError>> =
        let city, id, cd, ems = credentials.Value
        let baseUrl = createBaseUrl city
        let queryParams = createQueryParams id cd ems

        Web.Core.createClient <| Http baseUrl
        |> ResultAsync.wrap (fun client ->
            match client with
            | HttpClient client ->
                getStartPage client queryParams
                |> ResultAsync.bind (fun startpage -> Error(Logical(NotImplemented "getAppointments")))
            | _ -> async { return Error(Logical(NotSupported $"{client}")) })



    let getResponse storage (request: Request) ct =

        let updatedRequest =
            { request with
                Modified = DateTime.UtcNow }

        let updateRequest () =
            Persistence.Repository.Command.Request.update storage updatedRequest ct

        async {
            match request.Data |> Map.tryFind "url" with
            | None -> return Error(Infrastructure(InvalidRequest "No url found in requests data."))
            | Some requestUrl ->
                match createCredentials requestUrl with
                | Error error -> return Error(Infrastructure error)
                | Ok credentials ->
                    match! getKdmidResponse credentials ct with
                    | Error(Infrastructure(InvalidRequest msg))
                    | Error(Infrastructure(InvalidResponse msg)) ->
                        match! updateRequest () with
                        | Ok _ -> return Error(Infrastructure(InvalidRequest msg))
                        | Error error -> return Error error

                    | Error error -> return Error error
                    | Ok response ->
                        match response with
                        | response when response.Appointments.Count = 0 -> return Ok None
                        | response -> return Ok <| Some response
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
