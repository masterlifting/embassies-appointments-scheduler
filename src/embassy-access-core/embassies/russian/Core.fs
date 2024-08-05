module internal EmbassyAccess.Embassies.Russian.Core

open System
open Infrastructure
open Infrastructure.Parser
open EmbassyAccess.Domain
open EmbassyAccess.Embassies.Russian.Domain

module Http =
    open Web.Http.Domain
    open Web.Http.Client

    let createHttpClient city =
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

    let createQueryParams id cd ems =
        match ems with
        | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
        | None -> $"id=%i{id}&cd=%s{cd}"

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

    type Deps =
        { HttpClient: Web.Http.Domain.Client
          getInitialPage: HttpGetStringRequest
          getCaptcha: HttpGetBytesRequest
          solveCaptcha: SolveCaptchaImage }

    let createDeps (deps: GetAppointmentsDeps) httpClient =
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

    let handle (deps: Deps) =
        fun queryParams ->

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

module private ValidationPage =

    type Deps =
        { HttpClient: Web.Http.Domain.Client
          postValidationPage: HttpPostStringRequest }

    let createDeps (deps: GetAppointmentsDeps) httpClient =
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

    let handle (deps: Deps) =
        fun queryParams formData ->

            // define
            let postRequest =
                let request, content = createRequest formData queryParams
                deps.postValidationPage request content

            let parseResponse = ResultAsync.bind parseResponse
            let prepareFormData = ResultAsync.map' prepareFormData

            // pipe
            deps.HttpClient |> postRequest |> parseResponse |> prepareFormData

module private AppointmentsPage =
    type Deps =
        { HttpClient: Web.Http.Domain.Client
          postAppointmentsPage: HttpPostStringRequest }

    let createDeps (deps: GetAppointmentsDeps) httpClient =
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
                         IsConfirmed = false
                         Description = Some window }
                | _ -> Error <| NotSupported $"Appointment date: {dateTime}."
            | _ -> Error <| NotSupported $"Appointment row: {value}."

        match data.IsEmpty with
        | true -> Ok Set.empty
        | false -> data |> Set.map parse |> Seq.roe |> Result.map Set.ofSeq

    let handle (deps: Deps) =
        fun queryParams formData ->

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

module private ConfirmationPage =

    type Deps =
        { HttpClient: Web.Http.Domain.Client
          postConfirmationPage: HttpPostStringRequest }

    let createDeps (deps: BookAppointmentDeps) httpClient =
        { HttpClient = httpClient
          postConfirmationPage = deps.postConfirmationPage }

    let private chooseAppointment (appointments: Appointment Set) option =
        match option with
        | FirstAvailable -> appointments |> Seq.tryHead
        | Appointment appointment -> appointments |> Seq.tryFind (fun x -> x.Value = appointment.Value)
        | _ -> None

    let private createRequest formData queryParamsId =

        let request =
            { Web.Http.Domain.Request.Path = $"/queue/spcalendar.aspx?bjo=%i{queryParamsId}"
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
        | false -> Ok data

    let private updateAppointment (appointment: Appointment) description =
        { appointment with
            IsConfirmed = true
            Description = Some description }

    let handle (deps: Deps) =
        fun queryParamsId option (appointments, formData) ->

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
                let createResult = ResultAsync.map (updateAppointment appointment)

                // pipe
                deps.HttpClient
                |> postRequest
                |> parseResponse
                |> parseConfirmation
                |> createResult

module internal Helpers =
    let createAppointmentsResult (request, appointments) =
        { request with
            Appointments = appointments }

    let createConfirmationResult (request, (appointment: Appointment)) =
        let appointments =
            request.Appointments
            |> Set.filter (fun x -> x.Value <> appointment.Value)
            |> Set.add appointment

        { request with
            Appointments = appointments }

    let checkCredentials (request, credentials) =
        let embassy = request.Embassy |> EmbassyAccess.Mapper.External.toEmbassy
        let city = credentials.City |> EmbassyAccess.Mapper.External.toCity

        match embassy.Country.City.Name = city.Name with
        | true -> Ok(request, credentials)
        | false ->
            let error =
                $"Embassy city '{embassy.Country.City.Name}' is not matched with the requested City '{city.Name}'."

            Error <| NotSupported error

    let updateRequest (request: Request) =
        let request =
            { request with
                Attempt = request.Attempt + 1
                Modified = DateTime.UtcNow }

        match request.Attempt = 20 with
        | true ->
            Error
            <| Cancelled "The request was cancelled due to the maximum number of attempts."
        | false -> Ok request

let getAppointments (deps: GetAppointmentsDeps) =
    fun (credentials: Credentials) ->

        let city, id, cd, ems = credentials.Value
        let queryParams = Http.createQueryParams id cd ems

        city
        |> Http.createHttpClient
        |> ResultAsync.wrap (fun httpClient ->

            // define
            let processInitialPage () =
                let deps = InitialPage.createDeps deps httpClient
                InitialPage.handle deps queryParams

            let processValidationPage =
                let deps = ValidationPage.createDeps deps httpClient
                ResultAsync.bind' (ValidationPage.handle deps queryParams)

            let processAppointmentsPage =
                let deps = AppointmentsPage.createDeps deps httpClient
                ResultAsync.bind' (AppointmentsPage.handle deps queryParams)

            // pipe
            let getAppointments =
                processInitialPage >> processValidationPage >> processAppointmentsPage

            // run
            getAppointments ())

let bookAppointment (deps: BookAppointmentDeps) =
    fun (option: ConfirmationOption) (credentials: Credentials) ->

        let city, id, cd, ems = credentials.Value
        let queryParams = Http.createQueryParams id cd ems

        city
        |> Http.createHttpClient
        |> ResultAsync.wrap (fun httpClient ->

            // define
            let processInitialPage () =
                let deps = InitialPage.createDeps deps.GetAppointmentsDeps httpClient
                InitialPage.handle deps queryParams

            let processValidationPage =
                let deps = ValidationPage.createDeps deps.GetAppointmentsDeps httpClient
                ResultAsync.bind' (ValidationPage.handle deps queryParams)

            let processAppointmentsPage =
                let deps = AppointmentsPage.createDeps deps.GetAppointmentsDeps httpClient
                ResultAsync.bind' (AppointmentsPage.handle deps queryParams)

            let processConfirmationPage =
                let deps = ConfirmationPage.createDeps deps httpClient
                ResultAsync.bind' (ConfirmationPage.handle deps id option)

            // pipe
            let bookAppointment =
                processInitialPage
                >> processValidationPage
                >> processAppointmentsPage
                >> processConfirmationPage

            // run
            bookAppointment ())
