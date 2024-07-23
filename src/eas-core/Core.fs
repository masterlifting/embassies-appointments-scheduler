module Eas.Core

open System
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

            let setResponseCookie = ResultAsync.bind (deps.HttpClient |> Http.setRequiredCookie)
            let parseResponse = ResultAsync.bind StartPage.parseResponse

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

                    let setCookie = ResultAsync.bind (deps.HttpClient |> Http.setSessionCookie)
                    let prepareResponse = ResultAsync.bind StartPage.prepareCaptchaImage
                    let solveCaptcha = ResultAsync.bind' deps.solveCaptchaImage
                    let prepareFormData = ResultAsync.map' (pageData |> StartPage.prepareFormData)
                    let buildFormData = ResultAsync.map' Http.buildFormData

                    // pipe
                    deps.HttpClient
                    |> getCaptchaRequest
                    |> setCookie
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

            let parseResponse = ResultAsync.bind ValidationPage.parseResponse
            let prepareFormData = ResultAsync.map' ValidationPage.prepareFormData

            // pipe
            deps.HttpClient |> postRequest |> parseResponse |> prepareFormData

    let private processAppointmentsPage (deps: AppointmentsPage.Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let formData = Http.buildFormData formData
                let request, content = AppointmentsPage.createRequest formData queryParams
                deps.postCalendarPage request content

            let parseResponse = ResultAsync.bind AppointmentsPage.parseResponse
            let parseAppointments = ResultAsync.bind AppointmentsPage.parseAppointments
            let createResult = ResultAsync.map (fun appointments -> appointments, formData)

            // pipe
            deps.HttpClient
            |> postRequest
            |> parseResponse
            |> parseAppointments
            |> createResult

    let private processConfirmationPage (deps: ConfirmationPage.Deps) =
        fun queryParamsId formData appointments option ->

            match ConfirmationPage.chooseAppointment appointments option with
            | None -> async { return Error <| NotFound "Appointment to book." }
            | Some appointment ->

                // define
                let postRequest =
                    let formData =
                        appointment.Value
                        |> ConfirmationPage.prepareFormData formData
                        |> Http.buildFormData

                    let request, content = ConfirmationPage.createRequest formData queryParamsId
                    deps.postConfirmationPage request content

                let parseResponse = ResultAsync.bind ConfirmationPage.parseResponse
                let parseConfirmation = ResultAsync.bind ConfirmationPage.parseConfirmation

                // pipe
                deps.HttpClient |> postRequest |> parseResponse |> parseConfirmation


    let private getAppointments (deps: GetAppointmentsDeps) =
        fun (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            city
            |> Http.createHttpClient
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let deps = StartPage.createDeps deps httpClient
                    processStartPage deps queryParams

                let processValidationPage =
                    let deps = ValidationPage.createDeps deps httpClient
                    ResultAsync.bind' (processValidationPage deps queryParams)

                let processAppointmentsPage =
                    let deps = AppointmentsPage.createDeps deps httpClient
                    ResultAsync.bind' (processAppointmentsPage deps queryParams)

                // pipe
                let getAppointments =
                    processStartPage >> processValidationPage >> processAppointmentsPage

                // run
                getAppointments ())

    let private bookAppointment (deps: BookAppointmentDeps) =
        fun (option: ConfirmationOption) (credentials: Credentials) ->

            let city, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            city
            |> Http.createHttpClient
            |> ResultAsync.wrap (fun httpClient ->

                // define
                let processStartPage () =
                    let deps = StartPage.createDeps deps.GetAppointmentsDeps httpClient
                    processStartPage deps queryParams

                let processValidationPage =
                    let deps = ValidationPage.createDeps deps.GetAppointmentsDeps httpClient
                    ResultAsync.bind' (processValidationPage deps queryParams)

                let processAppointmentsPage =
                    let deps = AppointmentsPage.createDeps deps.GetAppointmentsDeps httpClient
                    ResultAsync.bind' (processAppointmentsPage deps queryParams)

                let processConfirmationPage =
                    let deps = ConfirmationPage.createDeps deps httpClient

                    ResultAsync.bind' (fun (appointments, formData) ->
                        processConfirmationPage deps id formData appointments option)

                // pipe
                let bookAppointment =
                    processStartPage
                    >> processValidationPage
                    >> processAppointmentsPage
                    >> processConfirmationPage

                // run
                bookAppointment ())

    [<RequireQualifiedAccess>]
    module API =

        let createGetAppointmentsDeps = createGetAppointmentsDeps

        let createBookAppointmentDeps = createBookAppointmentDeps

        let getAppointments deps =

            let inline toResult request (appointments: Set<Appointment>, _) =
                match appointments.IsEmpty with
                | true -> None
                | false -> Some <| (request |> createAppointmentsResponse appointments)

            fun (request: Request) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Kdmid request url." }
                | Some url ->
                    url
                    |> createCredentials
                    |> ResultAsync.wrap (getAppointments deps)
                    |> ResultAsync.map (toResult request)

        let tryGetAppointments deps =

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
                            match! deps.getAppointments request with
                            | Error error -> return! innerLoop requestsTail (Some error)
                            | response -> return response
                }

            fun requests -> innerLoop requests None

        let bookAppointment deps =

            let inline toResult request description =
                request |> createConfirmationResponse description

            fun (request: Request) (option: ConfirmationOption) ->
                match request.Data |> Map.tryFind "url" with
                | None -> async { return Error <| NotFound "Kdmid request url." }
                | Some url ->
                    url
                    |> createCredentials
                    |> ResultAsync.wrap (bookAppointment deps option)
                    |> ResultAsync.map (toResult request)
