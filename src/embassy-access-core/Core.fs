module EmbassyAccess.Core

open System
open Infrastructure
open EmbassyAccess.Domain.Core.Internal

module Russian =
    open EmbassyAccess.Web.Core.Russian
    open EmbassyAccess.Domain.Core.Internal.Russian

    let private processInitialPage (deps: InitialPage.Deps) =
        fun queryParams ->

            // define
            let getRequest =
                let request = InitialPage.createRequest queryParams
                deps.getInitialPage request

            let setCookie = ResultAsync.bind (deps.HttpClient |> Http.setRequiredCookie)
            let parseResponse = ResultAsync.bind InitialPage.parseResponse

            // pipe
            deps.HttpClient
            |> getRequest
            |> setCookie
            |> parseResponse
            |> ResultAsync.bind' (fun pageData ->
                match pageData |> Map.tryFind "captchaUrlPath" with
                | None -> async { return Error <| NotFound "Captcha information on the Initial Page." }
                | Some urlPath ->

                    // define
                    let getCaptchaRequest =
                        let request =
                            deps.HttpClient |> InitialPage.createCaptchaRequest urlPath queryParams

                        deps.getCaptcha request

                    let setCookie = ResultAsync.bind (deps.HttpClient |> Http.setSessionCookie)
                    let prepareResponse = ResultAsync.bind InitialPage.prepareCaptchaImage
                    let solveCaptcha = ResultAsync.bind' deps.solveCaptcha
                    let prepareFormData = ResultAsync.map' (pageData |> InitialPage.prepareFormData)
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
                deps.postAppointmentsPage request content

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
        fun queryParamsId option (appointments, formData) ->

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
                let processInitialPage () =
                    let deps = InitialPage.createDeps deps httpClient
                    processInitialPage deps queryParams

                let processValidationPage =
                    let deps = ValidationPage.createDeps deps httpClient
                    ResultAsync.bind' (processValidationPage deps queryParams)

                let processAppointmentsPage =
                    let deps = AppointmentsPage.createDeps deps httpClient
                    ResultAsync.bind' (processAppointmentsPage deps queryParams)

                // pipe
                let getAppointments =
                    processInitialPage >> processValidationPage >> processAppointmentsPage

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
                let processInitialPage () =
                    let deps = InitialPage.createDeps deps.GetAppointmentsDeps httpClient
                    processInitialPage deps queryParams

                let processValidationPage =
                    let deps = ValidationPage.createDeps deps.GetAppointmentsDeps httpClient
                    ResultAsync.bind' (processValidationPage deps queryParams)

                let processAppointmentsPage =
                    let deps = AppointmentsPage.createDeps deps.GetAppointmentsDeps httpClient
                    ResultAsync.bind' (processAppointmentsPage deps queryParams)

                let processConfirmationPage =
                    let deps = ConfirmationPage.createDeps deps httpClient
                    ResultAsync.bind' (processConfirmationPage deps id option)

                // pipe
                let bookAppointment =
                    processInitialPage
                    >> processValidationPage
                    >> processAppointmentsPage
                    >> processConfirmationPage

                // run
                bookAppointment ())

    let private checkCredentials (request, credentials) =
        let embassy = request.Embassy |> Mapper.Core.External.toEmbassy
        let city = credentials.City |> Mapper.Core.External.toCity

        match embassy.Country.City.Name = city.Name with
        | true -> Ok(request, credentials)
        | false ->
            let error =
                $"Embassy city '{embassy.Country.City.Name}' is not matched with the requested City '{city.Name}'."

            Error <| NotSupported error

    let private updateRequest (request: Request) =
        let request =
            { request with
                Attempt = request.Attempt + 1
                Modified = DateTime.UtcNow }

        match request.Attempt = 20 with
        | true ->
            Error
            <| Cancelled "The request was cancelled due to the maximum number of attempts."
        | false -> Ok request

    [<RequireQualifiedAccess>]
    module API =

        let createGetAppointmentsDeps = createGetAppointmentsDeps

        let createBookAppointmentDeps = createBookAppointmentDeps

        let getAppointments deps =

            // define
            let inline updateRequest request =
                request
                |> updateRequest
                |> ResultAsync.wrap deps.updateRequest
                |> ResultAsync.map (fun _ -> request)

            let createCredentials =
                ResultAsync.bind (fun request ->
                    createCredentials request.Value
                    |> Result.bind (fun credentials ->
                        (request, credentials)
                        |> checkCredentials
                        |> Result.map (fun _ -> request, credentials)))

            let getAppointments =
                ResultAsync.bind' (fun (request, credentials) ->
                    getAppointments deps credentials
                    |> ResultAsync.map (fun (appointments, _) -> request, appointments))

            let createResult =
                ResultAsync.map' (fun (request, appointments: Set<Appointment>) ->
                    match appointments.IsEmpty with
                    | true -> None
                    | false -> Some <| (request |> createAppointmentsResponse appointments))

            // pipe
            fun request -> 
                request 
                |> updateRequest 
                |> createCredentials 
                |> getAppointments 
                |> createResult

        let bookAppointment deps =

            // define
            let inline updateRequest (request, option) =
                request
                |> updateRequest
                |> ResultAsync.wrap deps.GetAppointmentsDeps.updateRequest
                |> ResultAsync.map (fun _ -> request, option)

            let createCredentials =
                ResultAsync.bind (fun (request, option) ->
                    createCredentials request.Value
                    |> Result.bind (fun credentials ->
                        (request, credentials)
                        |> checkCredentials
                        |> Result.map (fun _ -> request, option, credentials)))

            let bookAppointment =
                ResultAsync.bind' (fun (request, option, credentials) ->
                    bookAppointment deps option credentials
                    |> ResultAsync.map (fun result -> request, result))

            let createResult =
                ResultAsync.map' (fun (request, result) -> request |> createConfirmationResponse result)

            // pipe
            fun request option ->
                (request, option)
                |> updateRequest
                |> createCredentials
                |> bookAppointment
                |> createResult

        let tryGetAppointments getAppointments =

            let rec innerLoop requests error =
                async {
                    match requests with
                    | [] ->
                        return
                            match error with
                            | Some error -> Error error
                            | None -> Ok None
                    | request :: requestsTail ->
                        match! getAppointments request with
                        | Error error -> return! innerLoop requestsTail (Some error)
                        | response -> return response
                }

            fun requests -> innerLoop requests None
