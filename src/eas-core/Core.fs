module Eas.Core

open System
open Infrastructure.DSL
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module Russian =
    open Eas.Web.Russian
    open Embassies.Russian

    let private processStartPage (deps: Http.StartPage.Deps) =
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

                    let prepareFormData =
                        let addData = pageData |> Http.StartPage.prepareFormData
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

    let private processValidationPage (deps: Http.ValidationPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = Http.ValidationPage.createRequest formData queryParams

                deps.postValidationPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseValidationPage

            let prepareFormData = ResultAsync.map' Http.ValidationPage.prepareFormData

            // pipe
            deps.HttpClient |> postRequest |> parseResponse |> prepareFormData

    let private processCalendarPage (deps: Http.CalendarPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let formData = Http.buildFormData formData
                let request, content = Http.CalendarPage.createRequest formData queryParams

                deps.postCalendarPage request content

            let parseResponse = ResultAsync.bind Parser.Html.parseCalendarPage

            let parseAppointments = ResultAsync.bind Http.CalendarPage.parseAppointments

            let createResult =
                let createResult appointments = (appointments, formData)
                ResultAsync.map createResult

            // pipe
            deps.HttpClient
            |> postRequest
            |> parseResponse
            |> parseAppointments
            |> createResult

    let private processConfirmationPage (deps: Http.ConfirmationPage.Deps) =
        fun queryParamsId appointments formData option ->

            match Http.ConfirmationPage.chooseAppointment appointments option with
            | None -> async { return Error <| NotFound "Appointment to book." }
            | Some appointment ->

                // define
                let postRequest =
                    let formData =
                        Http.ConfirmationPage.prepareFormData formData appointment.Description
                        |> Http.buildFormData

                    let request, content = Http.ConfirmationPage.createRequest formData queryParamsId

                    deps.postConfirmationPage request content

                let parseResponse = ResultAsync.bind Parser.Html.parseConfirmationPage

                let parseConfirmation = ResultAsync.bind Http.ConfirmationPage.parseConfirmation

                // pipe
                deps.HttpClient |> postRequest |> parseResponse |> parseConfirmation


    let private getAppointments (deps: GetResponseDeps) =
        fun (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            city
            |> Http.createHttpClient
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let startPageDeps = Http.StartPage.createDeps deps httpClient
                    processStartPage startPageDeps queryParams

                let processValidationPage =
                    let validationPageDeps = Http.ValidationPage.createDeps deps httpClient

                    let process' formData =
                        processValidationPage validationPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processCalendarPage =
                    let calendarPageDeps = Http.CalendarPage.createDeps deps httpClient

                    let process' formData =
                        processCalendarPage calendarPageDeps queryParams formData

                    ResultAsync.bind' process'

                // pipe
                let getAppointments =
                    processStartPage >> processValidationPage >> processCalendarPage

                // run
                getAppointments ())

    let private bookAppointment (deps: BookRequestDeps) =
        fun (option: AppointmentOption) (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            city
            |> Http.createHttpClient
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let startPageDeps = Http.StartPage.createDeps deps.GetResponseDeps httpClient
                    processStartPage startPageDeps queryParams

                let processValidationPage =
                    let validationPageDeps =
                        Http.ValidationPage.createDeps deps.GetResponseDeps httpClient

                    let process' formData =
                        processValidationPage validationPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processCalendarPage =
                    let calendarPageDeps = Http.CalendarPage.createDeps deps.GetResponseDeps httpClient

                    let process' formData =
                        processCalendarPage calendarPageDeps queryParams formData

                    ResultAsync.bind' process'

                let processConfirmationPage =
                    let confirmationPageDeps = Http.ConfirmationPage.createDeps deps httpClient

                    let process' (appointments, formData) =
                        processConfirmationPage confirmationPageDeps id appointments formData option

                    ResultAsync.bind' process'

                // pipe
                let bookAppointment =
                    processStartPage
                    >> processValidationPage
                    >> processCalendarPage
                    >> processConfirmationPage

                // run
                bookAppointment ())

    [<RequireQualifiedAccess>]
    module API =

        let createGetResponseDeps = Http.createGetResponseDeps

        let createBookRequestDeps = Http.createBookRequestDeps

        let getResponse deps =

            let inline toResult request (appointments: Set<Appointment>, _) =
                match appointments.IsEmpty with
                | true -> None
                | false -> Some <| (request |> Http.createResponse appointments)

            fun (request: Request) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Kdmid request url." }
                | Some url ->
                    url
                    |> createCredentials
                    |> ResultAsync.wrap (getAppointments deps)
                    |> ResultAsync.map (toResult request)

        let tryGetResponse deps =

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

            fun requests -> innerLoop requests None

        let bookRequest deps =

            let inline toResult request description =
                request |> Http.createConfirmation description

            fun (request: Request) (option: AppointmentOption) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Kdmid request url." }
                | Some url ->
                    url
                    |> createCredentials
                    |> ResultAsync.wrap (bookAppointment deps option)
                    |> ResultAsync.map (toResult request)
