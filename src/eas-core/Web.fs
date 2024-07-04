module Eas.WebClient

open System
open Infrastructure
open Infrastructure.Domain.Errors

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

        let fakeValidationPageValidResponse () =
            async { return Ok(System.String.Empty) }

        let fakeValidationPageInvalidResponse () =
            async { return Ok(System.String.Empty) }

        let fakeCalendarEmptyResponse () =
            async { return Ok(System.String.Empty) }

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
                            | Ok(Some code) when code.Contains "CodeImage" -> Some("captcha", code)
                            | _ -> None
                        | _ -> None)
                    |> Map.ofSeq
                    |> Ok)
            |> Result.bind (fun result ->
                let requiredKeys =
                    Set
                        [ "captcha"
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
