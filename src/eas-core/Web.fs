module Eas.WebClient

open Infrastructure.Dsl
open Infrastructure.Domain.Errors
open Web.Domain

module Http =
    open Web

    let send (client: Http.Client) (request: Http.Domain.Request) ct =
        match request with
        | Http.Domain.Request.Get(path, headers) -> Http.get client path headers ct
        | Http.Domain.Request.Post(path, data, headers) -> Http.post client path data headers ct

module Telegram =
    open Web

    let send (client: Telegram.Client) (request: Telegram.Domain.Internal.Request) ct =
        match request with
        | Telegram.Domain.Internal.Request.Message(chatId, text) -> Telegram.sendText chatId text ct
        | _ -> async { return Error(Logical(NotSupported $"{client}")) }

module Repository =
    let send client request ct =
        match client, request with
        | HttpClient client, Request.Http request -> Http.send client request ct |> ResultAsync.map Http
        | TelegramClient client, Request.Telegram request -> Telegram.send client request ct |> ResultAsync.map Telegram
        | _ -> async { return Error(Logical(NotSupported $"{client} or {request}")) }

    let send'<'a> client request ct =
        match client, request with
        | HttpClient client, Request.Http request ->
            Http.send client request ct
            |> ResultAsync.bind (fun response ->
                let content, _ = response
                SerDe.Json.deserialize<'a> content |> Result.mapError Infrastructure)
        | TelegramClient client, Request.Telegram request ->
            Telegram.send client request ct
            |> ResultAsync.bind (fun response -> SerDe.Json.deserialize<'a> response |> Result.mapError Infrastructure)
        | _ -> async { return Error(Logical(NotSupported $"{client} or {request}")) }

    let receive client request ct =
        match client with
        | _ -> async { return Error(Logical(NotSupported $"{client}")) }

module Parser =
    module Html =
        open HtmlAgilityPack
        
        let parseStartPage (html: string) : Result<Map<string, string> * string, ApiError> =
            let document = Web.Parser.Html.load html
            Error(Logical(NotImplemented "parseStartPage"))