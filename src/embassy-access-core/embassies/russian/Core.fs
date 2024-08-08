module internal EmbassyAccess.Embassies.Russian.Core

open System
open Infrastructure
open Infrastructure.Parser
open EmbassyAccess.Domain
open EmbassyAccess.Embassies.Russian.Domain

module Http =
    open Web.Http.Domain
    open Web.Http.Client

    let private createClient' city =
        let host = $"%s{city}.kdmid.ru"

        let headers =
            Map
                [ "Host", [ host ]
                  "Accept",
                  [ "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" ]
                  "Accept-Language", [ "en-US,en;q=0.9,ru;q=0.8" ]
                  "Cache-Control", [ "max-age=0" ]
                  "Sec-Ch-Ua", [ "Not A(Brand\";v=\"99\", \"Microsoft Edge\";v=\"121\", \"Chromium\";v=\"121" ]
                  "Sec-Ch-Ua-Mobile", [ "?0" ]
                  "Sec-Ch-Ua-Platform", [ "\"Windows\"" ]
                  "Sec-Fetch-Dest", [ "document" ]
                  "Sec-Fetch-Mode", [ "navigate" ]
                  "Sec-Fetch-Site", [ "same-origin" ]
                  "Sec-Fetch-User", [ "?1" ]
                  "Upgrade-Insecure-Requests", [ "1" ]
                  "User-Agent",
                  [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36 Edg/121.0.0.0" ] ]
            |> Some

        create $"https://{host}" headers

    let createClient =
        ResultAsync.bind (fun credentials ->
            credentials.City.Name
            |> createClient'
            |> Result.map (fun httpClient -> httpClient, credentials))

    let createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

    let getQueryParamsId queryParams =
        queryParams
        |> Route.fromQueryParams
        |> Result.map (Map.tryFind "id")
        |> Result.bind (fun id ->
            match id with
            | Some id -> Ok id
            | None -> Error <| NotFound "Query parameter 'id'.")

    let private setCookie cookie httpClient =
        let headers = Map [ "Cookie", cookie ] |> Some
        httpClient |> Headers.set headers

    let setRequiredCookie httpClient (response: Response<string>) =
        response.Headers
        |> Headers.tryFind "Set-Cookie" [ "AlteonP"; "__ddg1_" ]
        |> Option.map (fun cookie -> httpClient |> setCookie cookie |> Result.map (fun _ -> response.Content))
        |> Option.defaultValue (Ok response.Content)

    let setSessionCookie httpClient (response: Response<byte array>) =
        response.Headers
        |> Headers.tryFind "Set-Cookie" [ "ASP.NET_SessionId" ]
        |> Option.map (fun cookie -> httpClient |> setCookie cookie |> Result.map (fun _ -> response.Content))
        |> Option.defaultValue (Ok response.Content)

    let buildFormData data =
        data
        |> Map.add "__EVENTTARGET" ""
        |> Map.add "__EVENTARGUMENT" ""
        |> Seq.map (fun x -> $"{Uri.EscapeDataString x.Key}={Uri.EscapeDataString x.Value}")
        |> String.concat "&"

let private hasError page =
    page
    |> Html.getNode "//span[@id='ctl00_MainContent_lblCodeErr']"
    |> Result.bind (fun error ->
        match error with
        | None -> Ok page
        | Some node ->
            match node.InnerText with
            | AP.IsString text ->
                Error
                <| Operation
                    { Message = text
                      Code = Some ErrorCodes.PageHasError }
            | _ -> Ok page)

module private InitialPage =
    open SkiaSharp

    type private Deps =
        { HttpClient: Web.Http.Domain.Client
          getInitialPage: HttpGetStringRequest
          getCaptcha: HttpGetBytesRequest
          solveCaptcha: SolveCaptchaImage }

    let private createDeps (deps: GetAppointmentsDeps) httpClient =
        { HttpClient = httpClient
          getInitialPage = deps.getInitialPage
          getCaptcha = deps.getCaptcha
          solveCaptcha = deps.solveCaptcha }

    let private createRequest queryParams =
        { Web.Http.Domain.Request.Path = "/queue/orderinfo.aspx?" + queryParams
          Web.Http.Domain.Request.Headers = None }

    let private parseResponse page =
        Html.load page
        |> Result.bind hasError
        |> Result.bind (Html.getNodes "//input | //img")
        |> Result.bind (fun nodes ->
            match nodes with
            | None -> Error <| NotFound "Nodes on the Initial Page."
            | Some nodes ->
                nodes
                |> Seq.choose (fun node ->
                    match node.Name with
                    | "input" ->
                        match node |> Html.getAttributeValue "name", node |> Html.getAttributeValue "value" with
                        | Ok(Some name), Ok(Some value) -> Some(name, value)
                        | Ok(Some name), Ok(None) -> Some(name, String.Empty)
                        | _ -> None
                    | "img" ->
                        match node |> Html.getAttributeValue "src" with
                        | Ok(Some code) when code.Contains "CodeImage" -> Some("captchaUrlPath", code)
                        | _ -> None
                    | _ -> None)
                |> Map.ofSeq
                |> Ok)
        |> Result.bind (fun result ->
            let requiredKeys =
                Set
                    [ "captchaUrlPath"
                      "__VIEWSTATE"
                      "__EVENTVALIDATION"
                      "ctl00$MainContent$txtID"
                      "ctl00$MainContent$txtUniqueID"
                      "ctl00$MainContent$ButtonA" ]

            let notRequiredKeys = Set [ "__VIEWSTATEGENERATOR" ]

            let requiredResult = result |> Map.filter (fun key _ -> requiredKeys.Contains key)

            let notRequiredResult =
                result |> Map.filter (fun key _ -> notRequiredKeys.Contains key)

            match requiredKeys.Count = requiredResult.Count with
            | true -> Ok(requiredResult |> Map.combine <| notRequiredResult)
            | false -> Error <| NotFound "Initial Page headers.")

    let private createCaptchaRequest urlPath =
        { Web.Http.Domain.Request.Path = $"/queue/{urlPath}"
          Web.Http.Domain.Request.Headers = None }

    let private prepareCaptchaImage (image: byte array) =
        try
            if image.Length = 0 then
                Error <| NotFound "Captcha image is empty."
            else
                let bitmap = image |> SKBitmap.Decode
                let bitmapInfo = bitmap.Info
                let bitmapPixels = bitmap.GetPixels()

                use pixmap = new SKPixmap(bitmapInfo, bitmapPixels)

                if pixmap.Height = pixmap.Width then
                    Ok image
                else
                    let x = pixmap.Width / 3
                    let y = 0
                    let width = x * 2
                    let height = pixmap.Height

                    let subset = pixmap.ExtractSubset <| SKRectI(x, y, width, height)
                    let data = subset.Encode(SKEncodedImageFormat.Png, 100)

                    Ok <| data.ToArray()
        with ex ->
            Error <| NotSupported ex.Message

    let private prepareFormData pageData captcha =
        pageData
        |> Map.remove "captchaUrlPath"
        |> Map.add "ctl00$MainContent$txtCode" $"%i{captcha}"
        |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
        |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

    let private handle' (deps, queryParams) =

        // define
        let getRequest =
            let request = createRequest queryParams
            deps.getInitialPage request

        let setCookie = ResultAsync.bind (deps.HttpClient |> Http.setRequiredCookie)
        let parseResponse = ResultAsync.bind parseResponse

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
                    let request = createCaptchaRequest urlPath
                    deps.getCaptcha request

                let setCookie = ResultAsync.bind (deps.HttpClient |> Http.setSessionCookie)
                let prepareResponse = ResultAsync.bind prepareCaptchaImage
                let solveCaptcha = ResultAsync.bind' deps.solveCaptcha
                let prepareFormData = ResultAsync.map' (pageData |> prepareFormData)
                let buildFormData = ResultAsync.map' Http.buildFormData

                // pipe
                deps.HttpClient
                |> getCaptchaRequest
                |> setCookie
                |> prepareResponse
                |> solveCaptcha
                |> prepareFormData
                |> buildFormData)

    let handle deps =
        ResultAsync.bind' (fun (httpClient, credentials: Credentials) ->
            let deps = createDeps deps httpClient
            let _, id, cd, ems = credentials.Value
            let queryParams = Http.createQueryParams id cd ems

            handle' (deps, queryParams)
            |> ResultAsync.map (fun formData -> httpClient, queryParams, formData))

module private ValidationPage =

    type private Deps =
        { HttpClient: Web.Http.Domain.Client
          postValidationPage: HttpPostStringRequest }

    let private createDeps (deps: GetAppointmentsDeps) httpClient =
        { HttpClient = httpClient
          postValidationPage = deps.postValidationPage }

    let private createRequest formData queryParams =

        let request =
            { Web.Http.Domain.Request.Path = "/queue/orderinfo.aspx?" + queryParams
              Web.Http.Domain.Request.Headers = None }

        let content: Web.Http.Domain.RequestContent =
            Web.Http.Domain.String
                {| Data = formData
                   Encoding = Text.Encoding.ASCII
                   MediaType = "application/x-www-form-urlencoded" |}

        request, content

    let private hasConfirmationRequest page =
        page
        |> Html.getNode "//span[@id='ctl00_MainContent_Content']"
        |> Result.bind (fun request ->
            match request with
            | None -> Ok page
            | Some node ->
                match node.InnerText with
                | AP.IsString text ->
                    match text.Contains "Ваша заявка требует подтверждения" with
                    | true ->
                        Error
                        <| Operation
                            { Message = text
                              Code = Some ErrorCodes.NotConfirmed }
                    | false -> Ok page
                | _ -> Ok page)

    let private parseResponse page =
        Html.load page
        |> Result.bind hasError
        |> Result.bind hasConfirmationRequest
        |> Result.bind (Html.getNodes "//input")
        |> Result.bind (fun nodes ->
            match nodes with
            | None -> Error <| NotFound "Nodes on the Validation Page."
            | Some nodes ->
                nodes
                |> Seq.choose (fun node ->
                    match node |> Html.getAttributeValue "name", node |> Html.getAttributeValue "value" with
                    | Ok(Some name), Ok(Some value) -> Some(name, value)
                    | Ok(Some name), Ok(None) -> Some(name, String.Empty)
                    | _ -> None)
                |> Map.ofSeq
                |> Ok)
        |> Result.bind (fun result ->
            let requiredKeys = Set [ "__VIEWSTATE"; "__EVENTVALIDATION" ]

            let notRequiredKeys = Set [ "__VIEWSTATEGENERATOR" ]

            let requiredResult = result |> Map.filter (fun key _ -> requiredKeys.Contains key)

            let notRequiredResult =
                result |> Map.filter (fun key _ -> notRequiredKeys.Contains key)

            match requiredKeys.Count = requiredResult.Count with
            | true -> Ok(requiredResult |> Map.combine <| notRequiredResult)
            | false -> Error <| NotFound "Validation Page headers.")

    let private prepareFormData data =
        data
        |> Map.add "ctl00$MainContent$ButtonB.x" "100"
        |> Map.add "ctl00$MainContent$ButtonB.y" "20"
        |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
        |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

    let private handle' (deps, queryParams, formData) =

        // define
        let postRequest =
            let request, content = createRequest formData queryParams
            deps.postValidationPage request content

        let parseResponse = ResultAsync.bind parseResponse
        let prepareFormData = ResultAsync.map' prepareFormData

        // pipe
        deps.HttpClient |> postRequest |> parseResponse |> prepareFormData

    let handle deps =
        ResultAsync.bind' (fun (httpClient, queryParams, formData) ->
            let deps = createDeps deps httpClient

            handle' (deps, queryParams, formData)
            |> ResultAsync.map (fun formData -> httpClient, queryParams, formData))

module private AppointmentsPage =
    type private Deps =
        { HttpClient: Web.Http.Domain.Client
          postAppointmentsPage: HttpPostStringRequest }

    let private createDeps (deps: GetAppointmentsDeps) httpClient =
        { HttpClient = httpClient
          postAppointmentsPage = deps.postAppointmentsPage }

    let private createRequest formData queryParams =

        let request =
            { Web.Http.Domain.Request.Path = "/queue/orderinfo.aspx?" + queryParams
              Web.Http.Domain.Request.Headers = None }

        let content: Web.Http.Domain.RequestContent =
            Web.Http.Domain.String
                {| Data = formData
                   Encoding = Text.Encoding.ASCII
                   MediaType = "application/x-www-form-urlencoded" |}

        request, content

    let private parseResponse page =
        Html.load page
        |> Result.bind hasError
        |> Result.bind (Html.getNodes "//input[@type='radio']")
        |> Result.bind (fun nodes ->
            match nodes with
            | None -> Ok Map.empty
            | Some nodes ->
                nodes
                |> Seq.choose (fun node ->
                    match node |> Html.getAttributeValue "name", node |> Html.getAttributeValue "value" with
                    | Ok(Some name), Ok(Some value) -> Some(value, name)
                    | _ -> None)
                |> List.ofSeq
                |> fun list ->
                    match list.Length = 0 with
                    | true -> Error <| NotFound "Appointments Page items."
                    | false -> list |> Map.ofList |> Ok)
        |> Result.map (Map.filter (fun _ value -> value = "ctl00$MainContent$RadioButtonList1"))
        |> Result.map (Seq.map (_.Key) >> Set.ofSeq)

    let private parseAppointments (data: Set<string>) =

        let parse (value: string) =
            //ASPCLNDR|2024-07-26T09:30:00|22|Окно 5
            let parts = value.Split '|'

            match parts.Length with
            | 4 ->
                let dateTime = parts[1]
                let window = parts[3]

                let date = DateOnly.TryParse dateTime
                let time = TimeOnly.TryParse dateTime

                match date, time with
                | (true, date), (true, time) ->
                    Ok
                    <| { Value = value
                         Date = date
                         Time = time
                         Confirmation = None
                         Description = Some window }
                | _ -> Error <| NotSupported $"Appointment date: {dateTime}."
            | _ -> Error <| NotSupported $"Appointment row: {value}."

        match data.IsEmpty with
        | true -> Ok Set.empty
        | false -> data |> Set.map parse |> Seq.roe |> Result.map Set.ofSeq

    let private handle' (deps, queryParams, formData) =

        // define
        let postRequest =
            let formData = Http.buildFormData formData
            let request, content = createRequest formData queryParams
            deps.postAppointmentsPage request content

        let parseResponse = ResultAsync.bind parseResponse
        let parseAppointments = ResultAsync.bind parseAppointments
        let createResult = ResultAsync.map (fun appointments -> appointments, formData)

        // pipe
        deps.HttpClient
        |> postRequest
        |> parseResponse
        |> parseAppointments
        |> createResult

    let handle deps =
        ResultAsync.bind' (fun (httpClient, queryParams, formData) ->
            let deps = createDeps deps httpClient

            queryParams
            |> Http.getQueryParamsId
            |> ResultAsync.wrap (fun id ->
                handle' (deps, queryParams, formData)
                |> ResultAsync.map (fun (appointments, formData) -> appointments, httpClient, id, formData)))

module private ConfirmationPage =

    type private Deps =
        { HttpClient: Web.Http.Domain.Client
          postConfirmationPage: HttpPostStringRequest }

    let private createDeps (deps: BookAppointmentDeps) httpClient =
        { HttpClient = httpClient
          postConfirmationPage = deps.postConfirmationPage }

    let private chooseAppointment (appointments: Appointment Set) option =
        match option with
        | FirstAvailable -> appointments |> Seq.tryHead
        | Appointment appointment -> appointments |> Seq.tryFind (fun x -> x.Value = appointment.Value)
        | _ -> None

    let private createRequest formData queryParamsId =

        let request =
            { Web.Http.Domain.Request.Path = $"/queue/spcalendar.aspx?bjo=%s{queryParamsId}"
              Web.Http.Domain.Request.Headers = None }

        let content: Web.Http.Domain.RequestContent =
            Web.Http.Domain.String
                {| Data = formData
                   Encoding = Text.Encoding.ASCII
                   MediaType = "application/x-www-form-urlencoded" |}

        request, content

    let private parseResponse page =
        Html.load page |> Result.bind hasError |> Result.map (fun _ -> "")

    let private prepareFormData data value =
        data |> Map.add "ctl00$MainContent$TextBox1" value

    let private parseConfirmation (data: string) =
        match data.Length = 0 with
        | true -> Error <| NotFound "Confirmation data is empty."
        | false -> Ok { Description = data }

    let private createResult appointment confirmation =
        { appointment with
            Confirmation = Some confirmation }

    let private handle' (deps, queryParamsId, option, appointments, formData) =

        match chooseAppointment appointments option with
        | None -> async { return Error <| NotFound "Appointment to book." }
        | Some appointment ->

            // define
            let postRequest =
                let formData = appointment.Value |> prepareFormData formData |> Http.buildFormData

                let request, content = createRequest formData queryParamsId
                deps.postConfirmationPage request content

            let parseResponse = ResultAsync.bind parseResponse
            let parseConfirmation = ResultAsync.bind parseConfirmation
            let createResult = ResultAsync.map (createResult appointment)

            // pipe
            deps.HttpClient
            |> postRequest
            |> parseResponse
            |> parseConfirmation
            |> createResult

    let handle deps option =
        ResultAsync.bind' (fun (appointments, httpClient, id, formData) ->
            let deps = createDeps deps httpClient
            handle' (deps, id, option, appointments, formData))

module private Request =

    let private validateCredentials request credentials =
        match request.Embassy.Country.City.Name = credentials.City.Name with
        | true -> Ok credentials
        | false ->
            Error
            <| NotSupported
                $"Embassy city '{request.Embassy.Country.City.Name}' is not matched with the requested City '{credentials.City.Name}'."

    let createCredentials =
        ResultAsync.bind (fun request -> createCredentials request.Value |> Result.bind (validateCredentials request))

    let setAsRunning updateRequest request =
        { request with
            State = Running
            Modified = DateTime.UtcNow }
        |> updateRequest
        |> ResultAsync.map (fun _ -> request)

    let private setAttempt' request =
        match request.Modified.DayOfYear = DateTime.Today.DayOfYear, request.Attempt > 20 with
        | true, true ->
            Error
            <| Cancelled "The request was cancelled due to the maximum number of attempts for today."
        | false, true ->
            Ok
            <| { request with
                   Attempt = 1
                   Modified = DateTime.UtcNow }
        | _ ->
            Ok
            <| { request with
                   Attempt = request.Attempt + 1
                   Modified = DateTime.UtcNow }

    let setAttempt updateRequest request =
        ResultAsync.bind' (fun (httpClient, queryParams, formData) ->
            request
            |> setAttempt'
            |> ResultAsync.wrap updateRequest
            |> ResultAsync.map (fun _ -> httpClient, queryParams, formData))

    let complete appointments updateRequest request =
        { request with
            State = Completed
            Appointments = appointments
            Modified = DateTime.UtcNow }
        |> updateRequest
        |> ResultAsync.map (fun _ -> request)

    let fail (error: Error') updateRequest request =
        { request with
            State = Failed
            Description = Some error.Message
            Modified = DateTime.UtcNow }
        |> updateRequest
        |> ResultAsync.map (fun _ -> request)

    let replaceAppointment (appointment: Appointment) request =
        request.Appointments
        |> Set.filter (fun x -> x.Value <> appointment.Value)
        |> Set.add appointment

let getAppointments deps request =

    // define
    let setRequestRunningState () =
        request |> Request.setAsRunning deps.updateRequest

    let createRequestCredentials = Request.createCredentials

    let createHttpClient = Http.createClient

    let processInitialPage = InitialPage.handle deps

    let setRequestAttempt = request |> Request.setAttempt deps.updateRequest

    let processValidationPage = ValidationPage.handle deps

    let processAppointmentsPage =
        AppointmentsPage.handle deps
        >> ResultAsync.map (fun (appointments, _, _, _) -> appointments)

    let completeRequest appointmentsRes =
        async {
            match! appointmentsRes with
            | Error error -> return! request |> Request.fail error deps.updateRequest
            | Ok appointments -> return! request |> Request.complete appointments deps.updateRequest
        }

    // pipe
    let run =
        setRequestRunningState
        >> createRequestCredentials
        >> createHttpClient
        >> processInitialPage
        >> setRequestAttempt
        >> processValidationPage
        >> processAppointmentsPage
        >> completeRequest
    
    run ()

let bookAppointment deps option request =

    // define
    let setRequestRunningState () =
        request |> Request.setAsRunning deps.GetAppointmentsDeps.updateRequest

    let createRequestCredentials = Request.createCredentials

    let createHttpClient = Http.createClient

    let processInitialPage = InitialPage.handle deps.GetAppointmentsDeps

    let setRequestAttempt =
        request |> Request.setAttempt deps.GetAppointmentsDeps.updateRequest

    let processValidationPage = ValidationPage.handle deps.GetAppointmentsDeps

    let processAppointmentsPage = AppointmentsPage.handle deps.GetAppointmentsDeps

    let processConfirmationPage = ConfirmationPage.handle deps option

    let completeRequest appointmentRes =
        async {
            match! appointmentRes with
            | Error error -> return! request |> Request.fail error deps.GetAppointmentsDeps.updateRequest
            | Ok appointment ->
                return!
                    request
                    |> Request.replaceAppointment appointment
                    |> fun appointments ->
                        request |> Request.complete appointments deps.GetAppointmentsDeps.updateRequest
        }

    // pipe
    let run =
        setRequestRunningState
        >> createRequestCredentials
        >> createHttpClient
        >> processInitialPage
        >> setRequestAttempt
        >> processValidationPage
        >> processAppointmentsPage
        >> processConfirmationPage
        >> completeRequest
    
    run ()
