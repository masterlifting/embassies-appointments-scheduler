module Eas.WebClient

open Infrastructure.Domain.Errors

module Parser =
    module Html =
        open HtmlAgilityPack
        open Infrastructure.DSL.AP

        let private hasError (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectSingleNode("//div[@class='error_msg']") with
                | null -> Ok html
                | error ->
                    match error.InnerText with
                    | IsString msg -> Error <| Business msg
                    | _ -> Ok html
            with ex ->
                Error <| Parsing ex.Message

        let private getNode (xpath: string) (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectSingleNode(xpath) with
                | null -> Ok None
                | node -> Ok <| Some node
            with ex ->
                Error <| Parsing ex.Message

        let private getNodes (xpath: string) (html: HtmlDocument) =
            try
                match html.DocumentNode.SelectNodes(xpath) with
                | null -> Ok None
                | nodes -> Ok <| Some nodes
            with ex ->
                Error <| Parsing ex.Message

        let private getAttributeValue (attribute: string) (node: HtmlNode) =
            try
                match node.GetAttributeValue(attribute, "") with
                | "" -> Ok None
                | value -> Ok <| Some value
            with ex ->
                Error <| Parsing ex.Message

        let fakeStartPageResponse () =
            async { return Ok(System.String.Empty) }

        let private fakeValidationPageRequestFormData =
            """
            __EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE=%2FwEPDwUKMTcyNjMyOTQ4Nw9kFgJmD2QWAgIFD2QWAgIBDxYCHghvbnN1Ym1pdAUMV2FpdEFuc3dlcigpFgICAw9kFgQCBg8PFgIeDEVycm9yTWVzc2FnZQUMRXJyb3JNZXNzYWdlZGQCEg8PFgIeCEltYWdlVXJsBRh%2BL0NvZGVJbWFnZS5hc3B4P2lkPWM1ODdkZGTDomlNthuqZYfK8FUpAll%2BTK5t%2Fg%3D%3D&__VIEWSTATEGENERATOR=EE4D9765&__EVENTVALIDATION=%2FwEWBwKt4%2F%2BDAQLmjdfGDQKLs7ufCwK5ysLjCwKj8MqYCAKyrYjZCwLUs%2BeuC7R8mvWPnkPXWPQusogw%2BTtUFhoU&ctl00%24MainContent%24txtID=20781&ctl00%24MainContent%24txtUniqueID=F23CB539&ctl00%24MainContent%24txtCode=884343&ctl00%24MainContent%24ButtonA=%D0%94%D0%B0%D0%BB%D0%B5%D0%B5&ctl00%24MainContent%24FeedbackClientID=0&ctl00%24MainContent%24FeedbackOrderID=0
            """

        let fakeValidationPageValidResponse () =
            async { return Ok(System.String.Empty) }

        let fakeValidationPageInvalidResponse () =
            async { return Ok(System.String.Empty) }

        let private fakeCalendarRequestFormData =
            """
            __EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE=%2FwEPDwUKMTcyNjMyOTQ4Nw9kFgJmD2QWAgIFD2QWAgIBDxYCHghvbnN1Ym1pdAUMV2FpdEFuc3dlcigpFgQCAw8WAh4HVmlzaWJsZWgWDgIFDw8WAh4EVGV4dAUFMjA3ODFkZAIGDw8WBB8CZR4MRXJyb3JNZXNzYWdlBQxFcnJvck1lc3NhZ2VkZAILDw8WAh8CBQhGMjNDQjUzOWRkAhIPDxYCHghJbWFnZVVybAUYfi9Db2RlSW1hZ2UuYXNweD9pZD1jMDY2ZGQCFA8PFgIfAgUGMTczODY3ZGQCFg8PFgQfAgWCAdCh0LjQvNCy0L7Qu9GLINGBINC60LDRgNGC0LjQvdC60Lgg0LLQstC10LTQtdC90Ysg0L3QtdC%2F0YDQsNCy0LjQu9GM0L3Qvi4g0J%2FQvtC20LDQu9GD0LnRgdGC0LAsINC%2F0L7QstGC0L7RgNC40YLQtSDQv9C%2B0L%2FRi9GC0LrRgy4fAWhkZAIYDw8WAh8BaGRkAgUPFgIfAWcWBAIBDw8WAh8CBbgC0KPQstCw0LbQsNC10LzRi9C5INCf0LXRgdGC0YPQvdC%2B0LIg0JDQvdC00YDQtdC5INCS0LjQutGC0L7RgNC%2B0LLQuNGHPGJyIC8%2BPGJyIC8%2B0JLRiyDQt9Cw0YDQtdCz0LjRgdGC0YDQuNGA0L7QstCw0L3RiyDQsiDRgdC40YHRgtC10LzQtSDQv9C%2BINCy0L7Qv9GA0L7RgdGDIDxiciAvPtCR0LjQvtC80LXRgtGA0LjRh9C10YHQutC40Lkg0L%2FQsNGB0L%2FQvtGA0YIg0YDQtdCx0LXQvdC60YM8YnIgLz7QndC%2B0LzQtdGAINC30LDRj9Cy0LrQuCAtICAyMDc4MTxiciAvPtCX0LDRidC40YLQvdGL0Lkg0LrQvtC0IC0gIEYyM0NCNTM5PGJyIC8%2BPGJyIC8%2BZGQCAw8PFgIfAWdkZBgBBR5fX0NvbnRyb2xzUmVxdWlyZVBvc3RCYWNrS2V5X18WAQUZY3RsMDAkTWFpbkNvbnRlbnQkQnV0dG9uQnBPZxOT1CQyQhQHmtk3Dh0HD6P9&__VIEWSTATEGENERATOR=EE4D9765&__EVENTVALIDATION=%2FwEWBALtsMjDBAKk8MqYCAKyrYjZCwLUs%2BeuCxfpJAgdDfYq6DZyCTyIo9sB2JJS&ctl00%24MainContent%24ButtonB.x=178&ctl00%24MainContent%24ButtonB.y=23&ctl00%24MainContent%24FeedbackClientID=0&ctl00%24MainContent%24FeedbackOrderID=0
            """

        let fakeCalendarEmptyResponse () =
            async { return Ok(System.String.Empty) }


        let parseStartPage page =
            Web.Parser.Html.load page
            |> Result.bind hasError
            |> Result.bind (getNodes "//input | //img")
            |> Result.bind (fun nodes ->
                match nodes with
                | None -> Error <| Business "No nodes found on the start page."
                | Some nodes ->
                    nodes
                    |> Seq.choose (fun node ->
                        match node.Name with
                        | "input" ->
                            match node |> getAttributeValue "name", node |> getAttributeValue "value" with
                            | Ok(Some name), Ok(Some value) -> Some(name, value)
                            | Ok(Some name), Ok(None) -> Some(name, System.String.Empty)
                            | _ -> None
                        | "img" ->
                            match node |> getAttributeValue "src" with
                            | Ok(Some code) when code.Contains("CodeImage") -> Some("captcha", code)
                            | _ -> None
                        | _ -> None)
                    |> Map.ofSeq
                    |> Ok)
            |> Result.bind (fun result ->
                let requiredKeys =
                    [ "__EVENTTARGET"
                      "__EVENTARGUMENT"
                      "__VIEWSTATE"
                      "__VIEWSTATEGENERATOR"
                      "__EVENTVALIDATION"
                      "ctl00$MainContent$txtID"
                      "ctl00$MainContent$txtUniqueID"
                      "ctl00$MainContent$txtCode"
                      "ctl00$MainContent$ButtonA"
                      "ctl00$MainContent$FeedbackClientID"
                      "ctl00$MainContent$FeedbackOrderID" ]

                let difference = requiredKeys |> List.except (result.Keys |> List.ofSeq)

                match difference.Length > 0 with
                | true ->
                    Error
                    <| Business $"No required data found on the start page response. Missing keys: {difference}"
                | false -> Ok result)

        let parseValidationPage page =
            Web.Parser.Html.load page
            |> Result.bind hasError
            |> Result.bind (getNodes "//input")
            |> Result.bind (fun nodes ->
                match nodes with
                | None -> Error <| Business "No nodes found on the validation page."
                | Some nodes ->
                    nodes
                    |> Seq.choose (fun node ->
                        match node |> getAttributeValue "name", node |> getAttributeValue "value" with
                        | Ok(Some name), Ok(Some value) -> Some(name, value)
                        | _ -> None)
                    |> Map.ofSeq
                    |> Ok)
            |> Result.bind (fun result ->
                match result.Count < 4 with
                | true -> Error <| Business "No required data found on the start page response."
                | false -> Ok result)
