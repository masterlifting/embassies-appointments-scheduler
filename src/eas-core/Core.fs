module Eas.Core

open System
open Infrastructure.DSL
open Infrastructure.DSL.CE
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Web.Domain
    open Eas.Web.Russian
    open Embassies.Russian

    type private StartPageDeps =
        { HttpClient: Http.Client
          getStartPage: GetStringRequest
          getCaptchaImage: GetBytesRequest
          solveCaptchaImage: SolveCaptchaImage }

    let private processStartPage deps =
        fun queryParams ->

            // define
            let getRequest =
                let request = Http.StartPage.createRequest queryParams
                deps.getStartPage request

            let setResponseCookie =
                let setCookie = deps.HttpClient |> Http.setRequiredCookie
                ResultAsync.bind setCookie

            let parseResponse = ResultAsync.bind Parser.Html.parseStartPage

            // pipe
            deps.HttpClient
            |> getRequest
            |> setResponseCookie
            |> parseResponse
            |> ResultAsync.bind' (fun pageData ->
                match pageData |> Map.tryFind "captchaUrlPath" with
                | None -> async { return Error <| NotFound "Captcha information on the Start Page." }
                | Some urlPath ->

                    // define
                    let getCaptchaRequest =
                        let request =
                            deps.HttpClient |> Http.StartPage.createCaptchaImageRequest urlPath queryParams

                        deps.getCaptchaImage request

                    let setResponseCookie =
                        let setCookie = deps.HttpClient |> Http.setSessionCookie
                        ResultAsync.bind setCookie

                    let prepareResponse = ResultAsync.bind Http.StartPage.prepareCaptchaImage

                    let solveCaptcha = ResultAsync.bind' deps.solveCaptchaImage

                    let addFormData =
                        let addData = pageData |> Http.StartPage.addFormData
                        ResultAsync.map' addData

                    let buildFormData = ResultAsync.map' Http.buildFormData

                    // pipe
                    deps.HttpClient
                    |> getCaptchaRequest
                    |> setResponseCookie
                    |> prepareResponse
                    |> solveCaptcha
                    |> addFormData
                    |> buildFormData)


    type private ValidationPageDeps =
        { HttpClient: Http.Client
          postValidationPage: PostStringRequest }

    let private processValidationPage deps =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = Http.ValidationPage.createRequest formData queryParams

                deps.postValidationPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseValidationPage

            let addFormData = ResultAsync.map' Http.ValidationPage.addFormData

            let buildFormData = ResultAsync.map' Http.buildFormData

            // pipe
            deps.HttpClient |> postRequest |> parseResponse |> addFormData |> buildFormData


    type private CalendarPageDeps =
        { HttpClient: Http.Client
          Request: Request
          postCalendarPage: PostStringRequest
          getCalendarPage: GetStringRequest }

    let private processCalendarPage deps =
        fun queryParamsId queryParams formData ->

            // define
            let postRequest =
                let request, content = Http.CalendarPage.createPostRequest formData queryParams

                deps.postCalendarPage request content

            let parsePostResponse = ResultAsync.map (fun (page, _) -> 
                deps.HttpClient)

            let getRequest =
                let request = Http.CalendarPage.createGetRequest queryParamsId

                ResultAsync.bind' (deps.getCalendarPage request)

            let parseGetResponse = ResultAsync.bind Parser.Html.parseCalendarPage

            let createResult =
                let createRequest = Http.CalendarPage.createResponse deps.Request
                ResultAsync.bind createRequest

            // pipe
            deps.HttpClient
            |> postRequest
            |> parsePostResponse
            |> getRequest
            |> parseGetResponse
            |> createResult


    let private postConfirmation (data: Map<string, string>) appointment queryParams : Async<Result<unit, Error'>> =
        async { return Error <| NotImplemented "postConfirmation" }

    let private searchResponse (deps: GetResponseDeps) request =
        fun (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            Http.createHttpClient city
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage =
                    processStartPage
                        { HttpClient = httpClient
                          getStartPage = deps.getStartPage
                          getCaptchaImage = deps.getCaptchaImage
                          solveCaptchaImage = deps.solveCaptchaImage }

                let processValidationPage =
                    processValidationPage
                        { HttpClient = httpClient
                          postValidationPage = deps.postValidationPage }

                let processCalendarPage =
                    processCalendarPage
                        { HttpClient = httpClient
                          Request = request
                          postCalendarPage = deps.postCalendarPage
                          getCalendarPage = deps.getCalendarPage }

                // pipe
                resultAsync {
                    let! startPageResult = processStartPage queryParams
                    let! validationPageResult = processValidationPage queryParams startPageResult
                    return processCalendarPage id queryParams validationPageResult
                })

    [<RequireQualifiedAccess>]
    module API =

        let createGetResponseDeps = Http.createGetResponseDeps

        let getResponse deps =

            let searchResponse = searchResponse deps

            let inline toResponseResult response =
                match response with
                | response when response.Appointments.IsEmpty -> None
                | response -> Some response

            fun (request: Request) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Url for Kdmid request." }
                | Some url ->
                    createCredentials url
                    |> ResultAsync.wrap (searchResponse request)
                    |> ResultAsync.map toResponseResult

        let tryGetResponse deps requests =

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

                        match! deps.updateRequest request with
                        | Error error -> return Error error
                        | _ ->
                            match! deps.getResponse request with
                            | Error error -> return! innerLoop requestsTail (Some error)
                            | response -> return response
                }

            innerLoop requests None
