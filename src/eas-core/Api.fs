module Eas.Api

open Infrastructure.Domain.Errors
open Infrastructure.Dsl
open Eas.Domain.Internal
open Eas.Persistence

let getSupportedEmbassies () =
    Set
    <| [ Russian <| Serbia Belgrade
         Russian <| Bosnia Sarajevo
         Russian <| Hungary Budapest
         Russian <| Montenegro Podgorica
         Russian <| Albania Tirana ]

let getRequests storage filter ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Query.Request.get storage filter ct)

let getResponses storage filter ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Query.Response.get storage filter ct)

let addRequest storage request ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Command.Request.execute storage (Command.Request.Add request) ct)

let updateRequest storage request ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Command.Request.execute storage (Command.Request.Update request) ct)

let deleteRequest storage request ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Command.Request.execute storage (Command.Request.Delete request) ct)

let addResponse storage response ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage -> Repository.Command.Response.execute storage (Command.Response.Add response) ct)

let updateResponse storage response ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage ->
        Repository.Command.Response.execute storage (Command.Response.Update response) ct)

let deleteResponse storage response ct =
    Repository.createStorage storage
    |> Result.mapError Infrastructure
    |> ResultAsync.wrap (fun storage ->
        Repository.Command.Response.execute storage (Command.Response.Delete response) ct)
