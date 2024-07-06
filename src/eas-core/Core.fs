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
        let headers =
            Map
                [ "Accept",
                  [ "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" ]
                  "Accept-Language", [ "en-US,en;q=0.9,ru;q=0.8" ]
                  "Cache-Control", [ "max-age=0" ]
                  "Sec-Ch-Ua", [ "Not A(Brand\";v=\"99\", \"Microsoft Edge\";v=\"121\", \"Chromium\";v=\"121" ]
                  "Sec-Ch-Ua-Mobile", [ "?0" ]
                  "Sec-Ch-Ua-Platform", [ "\"Windows\"" ]
                  "Sec-Fetch-Dest", [ "document" ]
                  "Sec-Fetch-Mode", [ "navigate" ]
                  "Sec-Fetch-User", [ "?1" ]
                  "Upgrade-Insecure-Requests", [ "1" ]
                  "User-Agent",
                  [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36 Edg/121.0.0.0" ] ]
            |> Some

        Http.create $"https://%s{city}.kdmid.ru" headers

    let private createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let private buildFormData (data: (string * string) list) =
        data
        |> Seq.map (fun (key, value) -> $"{Uri.EscapeDataString key}={Uri.EscapeDataString value}")
        |> String.concat "&"

    let private setCookie cookies httpClient =
        let headers = Map [ "Cookie", cookies ]
        httpClient |> Http.Headers.add (Some headers)

    let private setRequiredCookie httpClient (data: string, headers: Headers) =
        headers
        |> Http.Headers.find "Set-Cookie" [ "AlteonP"; "__ddg1_" ]
        |> Result.map (fun cookie ->
            httpClient |> setCookie cookie
            data)

    let private setSessionCookie httpClient (image: byte array, headers: Headers) =
        headers
        |> Http.Headers.find "Set-Cookie" [ "ASP.NET_SessionId" ]
        |> Result.map (fun cookie ->
            httpClient |> setCookie cookie
            image)

    let private getStartPageData getStartPage getCaptchaImage solveCaptchaImage httpClient =
        fun queryParams ->
            let request =
                { Path = "/queue/orderinfo.aspx?" + queryParams
                  Headers = None }

            httpClient
            |> getStartPage request
            |> ResultAsync.bind (httpClient |> setRequiredCookie)
            |> ResultAsync.bind WebClient.Parser.Html.parseStartPage
            |> ResultAsync.bind' (fun pageData ->
                match pageData |> Map.tryFind "captcha" with
                | None -> async { return Error <| NotFound "Captcha information on the Start Page." }
                | Some urlPath ->
                    let origin = httpClient |> Http.Route.toOrigin
                    let host = httpClient |> Http.Route.toHost

                    let headers =
                        Map
                            [ "Host", [ host ]
                              "Referer", [ (origin + "/queue/orderinfo.aspx?" + queryParams) ]
                              "Sec-Fetch-Site", [ "same-origin" ] ]

                    let request =
                        { Path = $"/queue/{urlPath}"
                          Headers = Some headers }

                    let addFormData captcha =
                        pageData
                        |> Map.remove "captcha"
                        |> Map.add "ctl00$MainContent$txtCode" $"%i{captcha}"
                        |> Map.add "__EVENTTARGET" ""
                        |> Map.add "__EVENTARGUMENT" ""
                        |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
                        |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

                    let formatDataOrderPattern =
                        [ "__EVENTTARGET"
                          "__EVENTARGUMENT"
                          "__VIEWSTATE"
                          "__VIEWSTATEGENERATOR"
                          "__EVENTVALIDATION"
                          "ctl00$MainContent$txtID"
                          "ctl00$MainContent$txtUniqueID"
                          "ctl00$MainContent$txtCode"
                          "ctl00$MainContent$ButtonA"
                          "ctl00$MainContent$FeedbackClientID"
                          "ctl00$MainContent$FeedbackOrderID" ]

                    let orderFormData (pattern: string list) (pageData: Map<string, string>) : (string * string) list =

                        let a = pattern |> List.map (fun key -> key, pageData[key])

                        a


                    httpClient
                    |> getCaptchaImage request
                    |> ResultAsync.bind (httpClient |> setSessionCookie)
                    |> ResultAsync.bind' solveCaptchaImage
                    |> ResultAsync.map' addFormData
                    |> ResultAsync.map' (orderFormData formatDataOrderPattern)
                    |> ResultAsync.map' buildFormData)

    let private getValidationPageData postValidationPage httpClient =
        fun formData queryParams ->

            let headers = Map [ "Origin", [ httpClient |> Http.Route.toOrigin ] ]

            let request =
                { Path = "/queue/orderinfo.aspx?" + queryParams
                  Headers = Some headers }

            let content =
                String
                    {| Data = formData
                       Encoding = Text.Encoding.ASCII
                       MediaType = "application/x-www-form-urlencoded" |}

            let addFormData data =
                data
                |> Map.add "ctl00$MainContent$ButtonA.x" "100"
                |> Map.add "ctl00$MainContent$ButtonA.y" "20"
                |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
                |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"
                |> Map.toList

            httpClient
            |> postValidationPage request content
            |> ResultAsync.bind WebClient.Parser.Html.parseValidationPage
            |> ResultAsync.map' addFormData
            |> ResultAsync.map' buildFormData

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
