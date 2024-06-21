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
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let private getStartPage (client: Web.Http.Client) queryParams (ct: Threading.CancellationToken) =
        let urlPath = "/queue/orderinfo.aspx?" + queryParams

        // Web.Http.get client urlPath None ct
        // |> ResultAsync.bind (fun (content, _) -> WebClient.Parser.Html.parseStartPage content)
        async {
            return WebClient.Parser.Html.parseStartPage WebClient.Parser.Html.test

        }

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


    let private getKdmidResponse (credentials: Credentials) ct : Async<Result<Domain.Internal.Response, ApiError>> =
        let city, id, cd, ems = credentials.Value
        let baseUrl = createBaseUrl city
        let queryParams = createQueryParams id cd ems

        Web.Http.create baseUrl
        |> ResultAsync.wrap (fun client ->
            async {
                let! startPageRes = getStartPage client queryParams ct

                return
                    match startPageRes with
                    | Error error -> Error error
                    | Ok data -> Error(Logical(NotImplemented $"{data}"))
            })

    let getResponse storage (request: Eas.Domain.Internal.Request) ct =
        match request.Data |> Map.tryFind "url" with
        | None -> async { return Error(Infrastructure(InvalidRequest "No url found in requests data.")) }
        | Some url ->
            createCredentials url
            |> Result.mapError Infrastructure
            |> ResultAsync.wrap (fun credentials ->
                getKdmidResponse credentials ct
                |> ResultAsync.map (fun response ->
                    match response with
                    | response when response.Appointments.IsEmpty -> None
                    | response -> Some response))

    let tryGetResponse requests updateRequest getResponse =

        let rec innerLoop requests error =
            async {
                match requests with
                | [] ->
                    return
                        match error with
                        | Some error -> Error error
                        | None -> Ok None
                | request :: requestsTail ->

                    let request: Domain.Internal.Request =
                        { request with
                            Modified = DateTime.UtcNow }

                    match! updateRequest request with
                    | Error error -> return Error error
                    | Ok _ ->
                        match! getResponse request with
                        | Error error -> return! innerLoop requestsTail (Some error)
                        | response -> return response
            }

        innerLoop requests None
