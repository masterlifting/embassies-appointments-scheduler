module Eas.Core

open System
open Infrastructure
open Infrastructure.Dsl
open Infrastructure.Dsl.ActivePatterns
open Infrastructure.Dsl.SerDe
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

    let private getCaptcha urlPath ct client =
        let request =
            { Path = $"/queue/{urlPath}"
              Headers = None }

        client
        |> Http.get' request ct
        |> ResultAsync.bind' (fun (image, _) -> Http.Captcha.AntiCaptcha.solveInt image ct)

    let private buildFormData (data: Map<string, string>) =
        data |> Seq.map (fun x -> $"{x.Key}={x.Value}") |> String.concat "%24"

    let private getStartPageRequestFormData queryParams ct client =
        let request =
            { Path = "/queue/OrderInfo.aspx?" + queryParams
              Headers = None }


        //client |> Http.get request ct
        WebClient.Parser.Html.fakeStartPageRequest ()
        |> ResultAsync.bind' (fun (content, _) ->
            async {
                match WebClient.Parser.Html.parseStartPageRequest content with
                | Error error -> return Error error
                | Ok pageData ->
                    match pageData |> Map.tryFind "captcha" with
                    | None -> return Error <| Web "No captcha data found in start page data."
                    | Some urlPath ->
                        Logging.Log.warning $"Captcha found: %s{urlPath}"
                        do! Async.Sleep 10000
                        Logging.Log.warning "Captcha solved"
                        match! client |> getCaptcha urlPath ct with
                        | Error error -> return Error error
                        | Ok captcha ->
                            return
                                Ok(
                                    pageData
                                    |> Map.filter (fun key _ -> key <> "captcha")
                                    |> Map.add "ctl00%24MainContent%24txtCode" (captcha |> string)
                                    |> buildFormData
                                )
            })

    let getStartPageResponseFormData formData queryParams ct client =
        let request =
            { Path = "/queue/OrderInfo.aspx?" + queryParams
              Headers = None }

        let content =
            String
                {| Content = formData
                   Encoding = Text.Encoding.ASCII
                   MediaType = "application/x-www-form-urlencoded" |}

        //client |> Http.post request content ct
        WebClient.Parser.Html.fakeStartPageResponse ()
        |> ResultAsync.bind (fun (content, _) ->
            match WebClient.Parser.Html.parseStartPageResponse content with
            | Error error -> Error error
            | Ok pageData -> Ok(pageData |> buildFormData))

    let private postCalendarPage
        (data: Map<string, string>)
        queryParams
        : Async<Result<(Map<string, string> * Set<Appointment>), Error'>> =
        //getStartPage baseUrl urlParams
        async { return Error <| NotImplemented "getCalendarPage" }

    let private postConfirmation (data: Map<string, string>) apointment queryParams : Async<Result<unit, Error'>> =
        async { return Error <| NotImplemented "postConfirmation" }

    let private getKdmidResponse configuration =
        fun (credentials: Credentials) ct ->
            let city, id, cd, ems = credentials.Value
            let queryParams = createQueryParams id cd ems

            createKdmidHttpClient city
            |> ResultAsync.wrap (fun client ->
                async {
                    match! client |> getStartPageRequestFormData queryParams ct with
                    | Error error -> return Error error
                    | Ok startPageFormData ->
                        match!  client |> getStartPageResponseFormData startPageFormData queryParams ct with
                        | Error error -> return Error error
                        | Ok calendarPageFormData -> return Error <| NotImplemented calendarPageFormData
                })

    let getResponse configuration =
        fun (request: Request) ct ->
            match request.Data |> Map.tryFind "url" with
            | None -> async { return Error <| Web "No url found in requests data." }
            | Some url ->
                createCredentials url
                |> ResultAsync.wrap (fun credentials ->
                    getKdmidResponse configuration credentials ct
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

                    let request: Request =
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
