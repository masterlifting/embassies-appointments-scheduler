module Eas.Core

open System
open Infrastructure
open Infrastructure.Dsl
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Embassies.Russian

    let private createKdmidHttpClient city =
        Web.Client.Http.create $"https://%s{city}.kdmid.ru" None

    let private createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let private solveCaptcha configuration =
        fun (image: byte[]) ct ->
            Web.Client.Http.create "https://api.anti-captcha.com" None
            |> ResultAsync.wrap (fun client ->
                async {
                    match Configuration.getEnvVar "AntiCaptchaKey" with
                    | Error error -> return Error error
                    | Ok None -> return Error <| Configuration "No AntiCaptcha key found in environment variables."
                    | Ok(Some key) ->
                        let body = System.Convert.ToBase64String image

                        let data =
                            {| ClientKey = key
                               Task =
                                {| Type = "ImageToTextTask"
                                   Body = body
                                   Phrase = false
                                   Case = false
                                   Numeric = 0
                                   Math = 0
                                   MinLength = 0
                                   MaxLength = 0 |} |}

                        let data =
                            Map.ofList
                                [ "clientKey", key
                                  "task",
                                  "{\"type\": \"ImageToTextTask\", \"body\": \"%s{body}\", \"phrase\": false, \"case\": false, \"numeric\": 0, \"math\": 0, \"minLength\": 0, \"maxLength\": 0}" ]
                        // let response = client |> Web.Http.postBytes "/createTask" data ct
                        return Error <| NotImplemented "solveCaptcha"
                })

    let private getCaptcha configuration =
        fun captchaUrlPath ct client ->
            let urlPath = "/queue/" + captchaUrlPath

            client
            |> Web.Client.Http.getBytes urlPath None ct
            |> ResultAsync.bind' (fun (image, _) -> solveCaptcha configuration image ct)


    let private getStartPage configuration client =
        fun queryParams ct ->
            let urlPath = "/queue/orderinfo.aspx?" + queryParams
            // Web.Http.get client urlPath None ct
            // |> ResultAsync.bind (fun (content, _) -> WebClient.Parser.Html.parseStartPage content)
            async {
                match WebClient.Parser.Html.parseStartPage WebClient.Parser.Html.test with
                | Error error -> return Error error
                | Ok startPageData ->
                    match startPageData |> Map.tryFind "captcha" with
                    | None -> return Error <| Web "No captcha found in start page data."
                    | Some captchaUrlPath ->
                        match! client |> getCaptcha configuration captchaUrlPath ct with
                        | Error error -> return Error error
                        | Ok captcha ->
                            return
                                Ok(
                                    startPageData
                                    |> Map.filter (fun key _ -> key <> "captcha")
                                    |> Map.add "ctl00%24MainContent%24txtCode" captcha
                                )
            }

    let postStartPage
        (data: Map<string, string>)
        (captcha: string)
        queryParams
        : Async<Result<Map<string, string>, Error'>> =
        async { return Error <| NotImplemented "postStartPage" }

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
                    let getStartPage = client |> getStartPage configuration

                    match! getStartPage queryParams ct with
                    | Error error -> return Error error
                    | Ok startPageData -> return Error <| NotImplemented $"{startPageData}"
                })

    let getResponse configuration =
        fun (request: Domain.Internal.Request) ct ->
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
