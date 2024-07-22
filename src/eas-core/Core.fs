module Eas.Core

open System
open Eas.Web.Russian.Http
open Infrastructure.DSL
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Eas.Web.Russian
    open Embassies.Russian

    let private processStartPage (deps: StartPage.Deps) =
        fun queryParams ->

            // define
            let getRequest =
                let request = StartPage.createRequest queryParams
                deps.getStartPage request

            let setResponseCookie =
                let setCookie = deps.HttpClient |> setRequiredCookie
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
                            deps.HttpClient |> StartPage.createCaptchaImageRequest urlPath queryParams

                        deps.getCaptchaImage request

                    let setResponseCookie =
                        let setCookie = deps.HttpClient |> setSessionCookie
                        ResultAsync.bind setCookie

                    let prepareResponse = ResultAsync.bind StartPage.prepareCaptchaImage

                    let solveCaptcha = ResultAsync.bind' deps.solveCaptchaImage

                    let prepareFormData =
                        let addData = pageData |> StartPage.prepareFormData
                        ResultAsync.map' addData

                    let buildFormData = ResultAsync.map' Http.buildFormData

                    // pipe
                    deps.HttpClient
                    |> getCaptchaRequest
                    |> setResponseCookie
                    |> prepareResponse
                    |> solveCaptcha
                    |> prepareFormData
                    |> buildFormData)

    let private processValidationPage (deps: ValidationPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = ValidationPage.createRequest formData queryParams

                deps.postValidationPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseValidationPage

            let prepareFormData = ResultAsync.map' ValidationPage.prepareFormData

            let buildFormData = ResultAsync.map' Http.buildFormData

            // pipe
            deps.HttpClient
            |> postRequest
            |> parseResponse
            |> prepareFormData
            |> buildFormData

    let private processCalendarPage (deps: CalendarPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = CalendarPage.createRequest formData queryParams

                deps.postCalendarPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseCalendarPage

            let prepareFormData = ResultAsync.map' CalendarPage.prepareFormData

            let buildFormData = ResultAsync.map' Http.buildFormData

            let createResult = ResultAsync.bind CalendarPage.getAppointments

            // pipe
            deps.HttpClient
            |> postRequest
            |> parseResponse
            |> prepareFormData
            |> buildFormData
            |> createResult

    let private processConfirmationPage (deps: ConfirmationPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = ConfirmationPage.createRequest formData queryParams

                deps.postConfirmationPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseConfirmationPage

            let getConfirmation = ResultAsync.bind ConfirmationPage.getConfirmation

            // pipe
            deps.HttpClient |> postRequest |> parseResponse |> getConfirmation

    let private getAppointments (deps: GetResponseDeps) =
        fun (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = createQueryParams id cd ems

            createHttpClient city
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let startPageDeps = StartPage.createDeps deps httpClient
                    processStartPage startPageDeps queryParams

                let processValidationPage =
                    let validationPageDeps = ValidationPage.createDeps deps httpClient

                    let process' formData =
                        processValidationPage validationPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processCalendarPage =
                    let calendarPageDeps = CalendarPage.createDeps deps httpClient

                    let process' formData =
                        processCalendarPage calendarPageDeps queryParams formData

                    ResultAsync.bind' process'

                // pipe
                let getAppointments =
                    processStartPage >> processValidationPage >> processCalendarPage

                getAppointments ())

    let private bookAppointment (deps: GetResponseDeps) =
        fun (credentials: Credentials) (appointment: Appointment) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = createQueryParams id cd ems

            createHttpClient city
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let startPageDeps = StartPage.createDeps deps httpClient
                    processStartPage startPageDeps queryParams

                let processValidationPage =
                    let validationPageDeps = ValidationPage.createDeps deps httpClient

                    let process' formData =
                        processValidationPage validationPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processCalendarPage =
                    let calendarPageDeps = CalendarPage.createDeps deps httpClient

                    let process' formData =
                        processCalendarPage calendarPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processConfirmationPage =
                    let confirmationPageDeps = ConfirmationPage.createDeps deps httpClient

                    let process' formData =
                        processConfirmationPage confirmationPageDeps queryParams formData

                    ResultAsync.bind' process'

                // pipe
                let getConfirmationResult =
                    processStartPage
                    >> processValidationPage
                    >> processCalendarPage
                    >> processConfirmationPage

                getConfirmationResult ())

    [<RequireQualifiedAccess>]
    module API =

        let createGetResponseDeps = Http.createGetResponseDeps

        let getResponse deps =

            let inline toResponse request (appointments: Set<Appointment>) =
                match appointments.IsEmpty with
                | true -> None
                | false -> Some <| (request |> createResponse appointments)

            fun (request: Request) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Kdmid request url." }
                | Some url ->
                    url
                    |> createCredentials
                    |> ResultAsync.wrap (getAppointments deps)
                    |> ResultAsync.map (toResponse request)

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
