module Eas.Core

open System
open Infrastructure.Dsl
open Infrastructure.Domain.Errors
open Web.Domain.Http
open Web.Client
open Eas.Domain.Internal

module Russian =
    open Embassies.Russian
    open Web.Client.Http.Captcha

    let private createKdmidHttpClient city =
        Http.create $"https://%s{city}.kdmid.ru" None

    let private createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let private solveCaptcha urlPath ct httpClient =
        let request =
            { Path = $"/queue/{urlPath}"
              Headers = None }

        httpClient
        |> Http.Request.Get.bytes request ct
        |> ResultAsync.bind' (AntiCaptcha.solveToInt ct)

    let private buildFormData (data: Map<string, string>) =
        data |> Seq.map (fun x -> $"{x.Key}={x.Value}") |> String.concat "%24"

    let private getStartPageData queryParams ct httpClient =
        let request =
            { Path = "/queue/OrderInfo.aspx?" + queryParams
              Headers = None }

        //httpClient |> Http.Request.Get.string request ct
        WebClient.Parser.Html.fakeStartPageResponse ()
        |> ResultAsync.bind WebClient.Parser.Html.parseStartPage
        |> ResultAsync.bind' (fun startPage ->
            match startPage |> Map.tryFind "captcha" with
            | None -> async { return Error <| NotFound "Captcha data for Kdmid request." }
            | Some urlPath ->
                httpClient
                |> solveCaptcha urlPath ct
                |> ResultAsync.map' (fun captcha ->
                    startPage
                    |> Map.filter (fun key _ -> key <> "captcha")
                    |> Map.add "ctl00%24MainContent%24txtCode" (captcha |> string)
                    |> buildFormData))

    let private getValidationPageData formData queryParams ct httpClient =
        let request =
            { Path = "/queue/OrderInfo.aspx?" + queryParams
              Headers = None }

        let content =
            String
                {| Data = formData
                   Encoding = Text.Encoding.ASCII
                   MediaType = "application/x-www-form-urlencoded" |}

        //httpClient |> Http.Request.Post.waitString request content ct
        WebClient.Parser.Html.fakeValidationPageValidResponse ()
        |> ResultAsync.bind WebClient.Parser.Html.parseValidationPage
        |> ResultAsync.map' buildFormData

    let private postCalendarPage
        (data: Map<string, string>)
        queryParams
        : Async<Result<(Map<string, string> * Set<Appointment>), Error'>> =
        //getStartPage baseUrl urlParams
        async { return Error <| NotImplemented "getCalendarPage" }

    let private postConfirmation (data: Map<string, string>) apointment queryParams : Async<Result<unit, Error'>> =
        async { return Error <| NotImplemented "postConfirmation" }

    let private getKdmidResponse configuration =
        fun ct (credentials: Credentials) ->
            let city, id, cd, ems = credentials.Value
            let queryParams = createQueryParams id cd ems

            createKdmidHttpClient city
            |> ResultAsync.wrap (fun httpClient ->
                async {
                    match! httpClient |> getStartPageData queryParams ct with
                    | Error error -> return Error error
                    | Ok startPageData ->
                        match! httpClient |> getValidationPageData startPageData queryParams ct with
                        | Error error -> return Error error
                        | Ok calendarPageFormData -> return Error <| NotImplemented calendarPageFormData
                })

    let getResponse configuration =
        let inline toResponseResult response =
            match response with
            | response when response.Appointments.IsEmpty -> None
            | response -> Some response

        fun (request: Request) ct ->
            match request.Data |> Map.tryFind "url" with
            | None -> async { return Error <| NotFound "Url for Kdmid request." }
            | Some url ->
                createCredentials url
                |> ResultAsync.wrap (getKdmidResponse configuration ct)
                |> ResultAsync.map toResponseResult

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

                    let request: Request =
                        { request with
                            Modified = DateTime.UtcNow }

                    match! updateRequest request with
                    | Error error -> return Error error
                    | _ ->
                        match! getResponse request with
                        | Error error -> return! innerLoop requestsTail (Some error)
                        | response -> return response
            }

        innerLoop requests None
