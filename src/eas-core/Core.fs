module Eas.Core

open System
open Infrastructure.DSL
open Infrastructure.DSL.CE
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Web.Domain
    open Embassies.Russian
    open Eas.Web.Russian

    type private StartPageDeps =
        { HttpClient: Http.Client
          getStartPage: Http.Request -> Http.Client -> Async<Result<string * Http.Headers, Error'>>
          getCaptchaImage: Http.Request -> Http.Client -> Async<Result<byte array * Http.Headers, Error'>>
          solveCaptchaImage: byte array -> Async<Result<int, Error'>> }

    let private getStartPageData deps =
        fun queryParams ->

            let getStartPage =
                let request = Http.createGetStartPageRequest queryParams
                deps.getStartPage request

            let setRequiredCookie = deps.HttpClient |> Http.setRequiredCookie

            deps.HttpClient
            |> getStartPage
            |> ResultAsync.bind setRequiredCookie
            |> ResultAsync.bind Parser.Html.parseStartPage
            |> ResultAsync.bind' (fun pageData ->
                match pageData |> Map.tryFind "captchaUrlPath" with
                | None -> async { return Error <| NotFound "Captcha information on the Start Page." }
                | Some urlPath ->

                    let getCaptchaImage =
                        let request =
                            deps.HttpClient |> Http.createGetCaptchaImageRequest urlPath queryParams

                        deps.getCaptchaImage request

                    let inline setSessionCookie image =
                        deps.HttpClient |> Http.setSessionCookie <| image

                    let solveCaptchaImage = deps.solveCaptchaImage

                    let inline addFormData captcha =
                        pageData |> Http.addStartPageFormData <| captcha

                    deps.HttpClient
                    |> getCaptchaImage
                    |> ResultAsync.bind setSessionCookie
                    |> ResultAsync.bind Http.prepareCaptchaImage
                    |> ResultAsync.bind' solveCaptchaImage
                    |> ResultAsync.map' addFormData
                    |> ResultAsync.map' Http.buildFormData)

    type private ValidationPageDeps =
        { HttpClient: Http.Client
          postValidationPage: Http.Request -> Http.RequestContent -> Http.Client -> Async<Result<string, Error'>> }

    let private getValidationPageData deps =
        fun formData queryParams ->

            let postValidationPage =
                let request, content =
                    deps.HttpClient |> Http.createPostValidationPageRequest formData queryParams

                deps.postValidationPage request content

            deps.HttpClient
            |> postValidationPage
            |> ResultAsync.bind Parser.Html.parseValidationPage
            |> ResultAsync.map' Http.addValidationPageFormData
            |> ResultAsync.map' Http.buildFormData

    let private postCalendarPage
        (data: Map<string, string>)
        queryParams
        : Async<Result<(Map<string, string> * Set<Appointment>), Error'>> =
        //getStartPage baseUrl urlParams
        async { return Error <| NotImplemented "getCalendarPage" }

    let private postConfirmation (data: Map<string, string>) apointment queryParams : Async<Result<unit, Error'>> =
        async { return Error <| NotImplemented "postConfirmation" }

    let private searchResponse (deps: GetResponseDeps) =
        fun (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            Http.createKdmidClient city
            |> ResultAsync.wrap (fun httpClient ->

                let getStartPageData =
                    getStartPageData
                        { HttpClient = httpClient
                          getStartPage = deps.getStartPage
                          getCaptchaImage = deps.getCaptchaImage
                          solveCaptchaImage = deps.solveCaptchaImage }

                let getValidationPageData =
                    getValidationPageData
                        { HttpClient = httpClient
                          postValidationPage = deps.postValidationPage }

                resultAsync {
                    let! startPageData = getStartPageData queryParams
                    let! getValidationPageData = getValidationPageData startPageData queryParams

                    return async { return Error <| NotImplemented "searchResponse" }
                })
    [<RequireQualifiedAccess>]
    module API =
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
                    |> ResultAsync.wrap searchResponse
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
