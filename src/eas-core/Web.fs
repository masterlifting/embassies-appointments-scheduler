module Eas.Web

open System
open Infrastructure.Domain.Errors

module Russian =

    module Http =
        open Web.Domain.Http
        open Web.Client

        [<Literal>]
        let private BasePath = "/queue/orderinfo.aspx?"

        let createKdmidClient city =
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

        let addStartPageFormData pageData captcha =
            pageData
            |> Map.remove "captcha"
            |> Map.add "ctl00$MainContent$txtCode" $"%i{captcha}"
            |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
            |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

        let addValidationPageFormData data =
            data
            |> Map.add "ctl00$MainContent$ButtonA.x" "100"
            |> Map.add "ctl00$MainContent$ButtonA.y" "20"
            |> Map.add "ctl00$MainContent$FeedbackClientID" "0"
            |> Map.add "ctl00$MainContent$FeedbackOrderID" "0"

        let buildFormData (data: Map<string, string>) =
            data
            |> Map.add "__EVENTTARGET" ""
            |> Map.add "__EVENTARGUMENT" ""
            |> Seq.map (fun x -> $"{Uri.EscapeDataString x.Key}={Uri.EscapeDataString x.Value}")
            |> String.concat "&"

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

        let createGetStartPageRequest queryParams =
            { Path = BasePath + queryParams
              Headers = None }

        let createGetCaptchaImageRequest urlPath queryParams httpClient =
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

        let createPostValidationPageRequest formData queryParams httpClient =
            let headers = Map [ "Origin", [ httpClient |> Http.Route.toOrigin ] ] |> Some

            let request =
                { Path = BasePath + queryParams
                  Headers = headers }

            let content =
                String
                    {| Data = formData
                       Encoding = Text.Encoding.ASCII
                       MediaType = "application/x-www-form-urlencoded" |}

            request, content

        open SkiaSharp
        open System.IO

        let prepareCaptchaImage (image: byte array) =

            try
                let bitmap = image |> SKBitmap.Decode
                let bitmapInfo = bitmap.Info
                let bitmapPixels = bitmap.GetPixels()

                use pixmap = new SKPixmap(bitmapInfo, bitmapPixels)

                let x = pixmap.Width / 3
                let y = 0
                let width = x * 2
                let height = pixmap.Height

                let subset = pixmap.ExtractSubset <| SKRectI(x, y, width, height)
                let data = subset.Encode(SKPngEncoderOptions.Default)

                Ok <| data.ToArray()
            with ex ->
                Error <| Operation { Message = ex.Message; Code = None }

    module Parser =
        module Html =
            open HtmlAgilityPack
            open Infrastructure.DSL.AP

            let private hasError (html: HtmlDocument) =
                try
                    match html.DocumentNode.SelectSingleNode("//span[@id='ctl00_MainContent_lblCodeErr']") with
                    | null -> Ok html
                    | error ->
                        match error.InnerText with
                        | IsString msg -> Error <| Operation { Message = msg; Code = None }
                        | _ -> Ok html
                with ex ->
                    Error <| NotSupported ex.Message

            let private getNode (xpath: string) (html: HtmlDocument) =
                try
                    match html.DocumentNode.SelectSingleNode(xpath) with
                    | null -> Ok None
                    | node -> Ok <| Some node
                with ex ->
                    Error <| NotSupported ex.Message

            let private getNodes (xpath: string) (html: HtmlDocument) =
                try
                    match html.DocumentNode.SelectNodes(xpath) with
                    | null -> Ok None
                    | nodes -> Ok <| Some nodes
                with ex ->
                    Error <| NotSupported ex.Message

            let private getAttributeValue (attribute: string) (node: HtmlNode) =
                try
                    match node.GetAttributeValue(attribute, "") with
                    | "" -> Ok None
                    | value -> Ok <| Some value
                with ex ->
                    Error <| NotSupported ex.Message

            let parseStartPage page =
                Web.Parser.Html.load page
                |> Result.bind hasError
                |> Result.bind (getNodes "//input | //img")
                |> Result.bind (fun nodes ->
                    match nodes with
                    | None -> Error <| NotFound "Nodes on the Start Page."
                    | Some nodes ->
                        nodes
                        |> Seq.choose (fun node ->
                            match node.Name with
                            | "input" ->
                                match node |> getAttributeValue "name", node |> getAttributeValue "value" with
                                | Ok(Some name), Ok(Some value) -> Some(name, value)
                                | Ok(Some name), Ok(None) -> Some(name, String.Empty)
                                | _ -> None
                            | "img" ->
                                match node |> getAttributeValue "src" with
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
                    | false -> Error <| NotFound "Required headers for Start Page.")

            let parseValidationPage page =
                Web.Parser.Html.load page
                |> Result.bind hasError
                |> Result.bind (getNodes "//input")
                |> Result.bind (fun nodes ->
                    match nodes with
                    | None -> Error <| NotFound "Nodes on the Validation Page."
                    | Some nodes ->
                        nodes
                        |> Seq.choose (fun node ->
                            match node |> getAttributeValue "name", node |> getAttributeValue "value" with
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
                    | false -> Error <| NotFound "Required headers for Start Page.")
