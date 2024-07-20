module Eas.Web

open System
open Infrastructure.Domain.Errors
open Eas.Domain.Internal

module internal Russian =
    open Eas.Domain.Internal.Embassies.Russian

    module Http =
        open Web.Domain.Http
        open Web.Client

        [<Literal>]
        let private BasePath = "/queue/orderinfo.aspx?"

        let createHttpClient city =
            let headers =
                Map
                    [ "Accept",
                      [ "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7" ]
                      "Accept-Language", [ "en-US,en;q=0.9,ru;q=0.8" ]
                      "Cache-Control", [ "max-age=0" ]
                      "Sec-Ch-Ua", [ "Not A(Brand\";v=\"99\", \"Microsoft Edge\";v=\"121\", \"Chromium\";v=\"121" ]
                      "Sec-Ch-Ua-Mobile", [ "?0" ]
                      "Sec-Ch-Ua-Platform", [ "\"Windows\"" ]
                      "Sec-Fetch-Dest", [ "document" ]
                      "Sec-Fetch-Mode", [ "navigate" ]
                      "Sec-Fetch-User", [ "?1" ]
                      "Upgrade-Insecure-Requests", [ "1" ]
                      "User-Agent",
                      [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/121.0.0.0 Safari/537.36 Edg/121.0.0.0" ] ]
                |> Some

            Http.create $"https://%s{city}.kdmid.ru" headers

        let createQueryParams id cd ems =
            match ems with
            | Some ems -> $"id=%i{id}&cd=%s{cd}&ems=%s{ems}"
            | None -> $"id=%i{id}&cd=%s{cd}"

        let private setCookie cookie httpClient =
            let headers = Map [ "Cookie", cookie ] |> Some
            httpClient |> Http.Headers.add headers

        let setRequiredCookie httpClient (data: string, headers: Headers) =
            headers
            |> Http.Headers.find "Set-Cookie" [ "AlteonP"; "__ddg1_" ]
            |> Result.map (fun cookie ->
                httpClient |> setCookie cookie
                data)

        let setSessionCookie httpClient (image: byte array, headers: Headers) =
            headers
            |> Http.Headers.find "Set-Cookie" [ "ASP.NET_SessionId" ]
            |> Result.map (fun cookie ->
                httpClient |> setCookie cookie
                image)

        let buildFormData data =
            data
            |> Map.add "__EVENTTARGET" ""
            |> Map.add "__EVENTARGUMENT" ""
            |> Seq.map (fun x -> $"{Uri.EscapeDataString x.Key}={Uri.EscapeDataString x.Value}")
            |> String.concat "&"

        let createGetResponseDeps ct =
            { getStartPage = Http.Request.Get.string' ct
              getCaptchaImage = Http.Request.Get.bytes' ct
              solveCaptchaImage = Web.Http.Captcha.AntiCaptcha.solveToInt ct
              postValidationPage = Http.Request.Post.waitString' ct
              postCalendarPage = Http.Request.Post.waitString' ct
              getCalendarPage = Http.Request.Get.string' ct }

        module StartPage =
            open SkiaSharp

            let createRequest queryParams =
                { Path = BasePath + queryParams
                  Headers = None }

            let createCaptchaImageRequest urlPath queryParams httpClient =
                let origin = httpClient |> Http.Route.toOrigin
                let host = httpClient |> Http.Route.toHost

                let headers =
                    Map
                        [ "Host", [ host ]
                          "Referer", [ origin + BasePath + queryParams ]
                          "Sec-Fetch-Site", [ "same-origin" ] ]
                    |> Some

                { Path = $"/queue/{urlPath}"
                  Headers = headers }

            let prepareCaptchaImage (image: byte array) =
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

            let addFormData pageData captcha =
                pageData
                |> Map.remove "captcha"
                |> Map.add "ctl00$MainContent$txtCode" $"%i{captcha}"
                |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
                |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

        module ValidationPage =
            let createRequest formData queryParams =

                let request =
                    { Path = BasePath + queryParams
                      Headers = None }

                let content =
                    String
                        {| Data = formData
                           Encoding = Text.Encoding.ASCII
                           MediaType = "application/x-www-form-urlencoded" |}

                request, content

            let addFormData data =
                data
                |> Map.add "ctl00$MainContent$ButtonA.x" "100"
                |> Map.add "ctl00$MainContent$ButtonA.y" "20"
                |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
                |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

        module CalendarPage =

            let createPostRequest formData queryParams =

                let request =
                    { Path = BasePath + queryParams
                      Headers = None }

                let content =
                    String
                        {| Data = formData
                           Encoding = Text.Encoding.ASCII
                           MediaType = "application/x-www-form-urlencoded" |}

                request, content

            let createGetRequest queryParamsId =

                let request =
                    { Path = $"/queue/spcalendar.aspx?bjo=%i{queryParamsId}"
                      Headers = None }

                request

            let createResponse request data =
                let date = data |> Map.tryFind "ctl00$MainContent$Calendar1$TextBox1"
                let time = data |> Map.tryFind "ctl00$MainContent$Calendar1$TextBox2"
                let appointment = data |> Map.tryFind "ctl00$MainContent$Calendar1$TextBox3"

                match date, time, appointment with
                | Some date, Some time, Some appointment ->
                    let date = DateOnly.Parse date
                    let time = TimeOnly.Parse time
                    let _ = DateTime.Parse appointment

                    let appointment =
                        { Id = Guid.NewGuid() |> AppointmentId
                          Date = date
                          Time = time
                          Description = "" }

                    Ok
                    <| { Id = Guid.NewGuid() |> ResponseId
                         Request = request
                         Appointments = Set.singleton appointment
                         Data = data
                         Modified = DateTime.Now }
                | _ -> Error <| NotFound "Appointments on the Calendar Page."

    module Parser =
        open Web.Parser.Client

        module Html =
            open Infrastructure.DSL.AP

            let private hasError page =
                page
                |> Html.getNode "//span[@id='ctl00_MainContent_lblCodeErr']"
                |> Result.bind (fun error ->
                    match error with
                    | None -> Ok page
                    | Some node ->
                        match node.InnerText with
                        | IsString text ->
                            Error
                            <| Operation
                                { Message = text
                                  Code = Some ErrorCodes.PageHasError }
                        | _ -> Ok page)

            let private hasConfirmationRequest page =
                page
                |> Html.getNode "//span[@id='ctl00_MainContent_Content']"
                |> Result.bind (fun request ->
                    match request with
                    | None -> Ok page
                    | Some node ->
                        match node.InnerText with
                        | IsString text ->
                            match text.Contains "Ваша заявка требует подтверждения" with
                            | true ->
                                Error
                                <| Operation
                                    { Message = text
                                      Code = Some ErrorCodes.NotConfirmed }
                            | false -> Ok page
                        | _ -> Ok page)

            let parseStartPage page =
                Html.load page
                |> Result.bind hasError
                |> Result.bind (Html.getNodes "//input | //img")
                |> Result.bind (fun nodes ->
                    match nodes with
                    | None -> Error <| NotFound "Nodes on the Start Page."
                    | Some nodes ->
                        nodes
                        |> Seq.choose (fun node ->
                            match node.Name with
                            | "input" ->
                                match
                                    node |> Html.getAttributeValue "name", node |> Html.getAttributeValue "value"
                                with
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
                              "__VIEWSTATEGENERATOR"
                              "__EVENTVALIDATION"
                              "ctl00$MainContent$txtID"
                              "ctl00$MainContent$txtUniqueID"
                              "ctl00$MainContent$ButtonA" ]

                    let result = result |> Map.filter (fun key _ -> requiredKeys.Contains key)

                    match requiredKeys.Count = result.Count with
                    | true -> Ok result
                    | false -> Error <| NotFound "Start Page headers.")

            let parseValidationPage (page, _) =
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
                    let requiredKeys =
                        Set [ "__VIEWSTATE"; "__VIEWSTATEGENERATOR"; "__EVENTVALIDATION" ]

                    let result = result |> Map.filter (fun key _ -> requiredKeys.Contains key)

                    match requiredKeys.Count = result.Count with
                    | true -> Ok result
                    | false -> Error <| NotFound "Validation Page headers.")

            let parseCalendarPage (page, _) =
                Html.load page
                |> Result.bind hasError
                |> Result.bind (Html.getNodes "//input")
                |> Result.bind (fun nodes ->
                    match nodes with
                    | None -> Error <| NotFound "Nodes on the Calendar Page."
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
                    let requiredKeys =
                        Set [ "__VIEWSTATE"; "__VIEWSTATEGENERATOR"; "__EVENTVALIDATION" ]

                    let result = result |> Map.filter (fun key _ -> requiredKeys.Contains key)

                    match requiredKeys.Count = result.Count with
                    | true -> Ok result
                    | false -> Error <| NotFound "Calendar Page headers.")
