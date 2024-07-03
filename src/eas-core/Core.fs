module Eas.Core

open System
open Infrastructure.DSL
open Infrastructure.DSL.CE
open Infrastructure.Domain.Errors
open Web.Domain.Http
open Web.Client
open Eas.Domain.Internal

module Russian =
    open Embassies.Russian

    let private createKdmidHttpClient city =
        Http.create $"https://%s{city}.kdmid.ru" None

    let private createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let private buildFormData (data: Map<string, string>) =
        data
        |> Map.add "__EVENTTARGET" String.Empty
        |> Map.add "__EVENTARGUMENT" String.Empty
        |> Seq.map (fun x -> $"{Uri.EscapeDataString x.Key}={Uri.EscapeDataString x.Value}")
        |> String.concat "&"

    let private getStartPageData getStartPage getCaptchaImage solveCaptchaImage httpClient =
        fun queryParams ->
            let request =
                { Path = "/queue/OrderInfo.aspx?" + queryParams
                  Headers = None }

            httpClient
            |> getStartPage request
            |> ResultAsync.bind WebClient.Parser.Html.parseStartPage
            |> ResultAsync.bind' (fun pageData ->
                match pageData |> Map.tryFind "captcha" with
                | None -> async { return Error <| NotFound "Captcha data for Kdmid request." }
                | Some urlPath ->
                    let request =
                        { Path = $"/queue/{urlPath}"
                          Headers = None }

                    httpClient
                    |> getCaptchaImage request
                    |> ResultAsync.map' (fun (image, headers) -> 
                        match headers with
                        | None -> image
                        | Some headers -> 

                        match headers |> Map.tryFind "Set-Cookie" with
                        | None -> image
                        | Some cookies -> 
                            image)
                    |> ResultAsync.bind' solveCaptchaImage
                    |> ResultAsync.map' (fun captcha ->
                        pageData
                        |> Map.filter (fun key _ -> key <> "captcha")
                        |> Map.add "ctl00$MainContent$txtCode" (captcha |> string)
                        |> buildFormData))

    let private getValidationPageData postValidationPage httpClient =
        fun formData queryParams ->
            let request =
                { Path = "/queue/OrderInfo.aspx?" + queryParams
                  Headers = None }

            let content =
                String
                    {| Data = formData
                       Encoding = Text.Encoding.ASCII
                       MediaType = "application/x-www-form-urlencoded" |}

            httpClient
            |> postValidationPage request content
            |> ResultAsync.bind WebClient.Parser.Html.parseValidationPage
            |> ResultAsync.map' (fun pageData ->
                pageData
                |> Map.add "ctl00$MainContent$ButtonB.x" "100"
                |> Map.add "ctl00$MainContent$ButtonB.y" "20"
                |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
                |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"
                |> buildFormData)

    let private postCalendarPage
        (data: Map<string, string>)
        queryParams
        : Async<Result<(Map<string, string> * Set<Appointment>), Error'>> =
        //getStartPage baseUrl urlParams
        async { return Error <| NotImplemented "getCalendarPage" }

    let private postConfirmation (data: Map<string, string>) apointment queryParams : Async<Result<unit, Error'>> =
        async { return Error <| NotImplemented "postConfirmation" }

    let private searchResponse props =
        fun (credentials: Credentials) ->
            let city, id, cd, ems = credentials.Value
            let queryParams = createQueryParams id cd ems

            createKdmidHttpClient city
            |> ResultAsync.wrap (fun httpClient ->
                let getStartPageData =
                    httpClient
                    |> getStartPageData props.getStartPage props.getCaptchaImage props.solveCaptchaImage

                let getValidationPageData =
                    httpClient |> getValidationPageData props.postValidationPage

                resultAsync {
                    let! startPageData = getStartPageData queryParams
                    let! getValidationPageData = getValidationPageData startPageData queryParams

                    return async { return Error <| NotImplemented "searchResponse" }
                })

    let getResponse props =
        let inline toResponseResult response =
            match response with
            | response when response.Appointments.IsEmpty -> None
            | response -> Some response

        fun (request: Request) ->
            match request.Data |> Map.tryFind "url" with
            | None -> async { return Error <| NotFound "Url for Kdmid request." }
            | Some url ->
                createCredentials url
                |> ResultAsync.wrap (searchResponse props)
                |> ResultAsync.map toResponseResult

    let tryGetResponse props requests =

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

                    match! props.updateRequest request with
                    | Error error -> return Error error
                    | _ ->
                        match! props.getResponse request with
                        | Error error -> return! innerLoop requestsTail (Some error)
                        | response -> return response
            }

        innerLoop requests None
