module Eas.Persistence

open Infrastructure
open Infrastructure.Dsl.SerDe
open Infrastructure.Dsl.Threading
open Infrastructure.Domain.Errors
open Persistence.Domain
open Persistence.Storage
open Domain
open Mapper

module QueryFilter =

    type Pagination = { Page: int; PageSize: int }

    type EmbassyFilter =
        { Pagination: Pagination
          Embassy: Internal.Embassy }

    type UserEmbassyFilter =
        { Pagination: Pagination
          User: Internal.User
          Embassy: Internal.Embassy }

    type User =
        | All
        | ByEmbassy

    type Request =
        | ByEmbassy of EmbassyFilter
        | ByUserEmbassy of UserEmbassyFilter

    type Response =
        | ByEmbassy of EmbassyFilter
        | ByUserEmbassy of UserEmbassyFilter

module Command =

    type Request =
        | Add of Internal.Request
        | Update of Internal.Request
        | Delete of Internal.Request

    type Response =
        | Add of Internal.Response
        | Update of Internal.Response
        | Delete of Internal.Response

module private InMemoryRepository =

    let private getData<'a> context key =
        context
        |> InMemory.get
        |> fun get -> get key
        |> Result.bind (Json.deserialize<'a array> |> Option.map >> Option.defaultValue (Ok [||]))

    module Query =
        open QueryFilter

        let paginate<'a> (data: 'a list) (pagination: Pagination) =
            data
            |> List.skip (pagination.Page * pagination.PageSize)
            |> List.take pagination.PageSize

        module Request =

            [<Literal>]
            let private key = "requests"

            let get context filter ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (requests: Internal.Request list) =
                                match filter with
                                | Request.ByEmbassy filter ->
                                    requests |> List.filter (fun x -> x.Embassy = filter.Embassy) |> paginate
                                    <| filter.Pagination
                                | Request.ByUserEmbassy filter ->
                                    requests
                                    |> List.filter (fun x -> x.User.Id = filter.User.Id && x.Embassy = filter.Embassy)
                                    |> paginate
                                    <| filter.Pagination

                            getData<External.Request> context key
                            |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)
                            |> Result.map filter
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Query.Request.get"))
                }

            let get' context requestId ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->

                            getData<External.Request> context key
                            |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)
                            |> Result.map (fun requests -> requests |> List.tryFind (fun x -> x.Id = requestId))
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Query.Request.get'"))
                }

        module Response =

            [<Literal>]
            let private key = "responses"

            let get context filter ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: Internal.Response list) =
                                match filter with
                                | Response.ByEmbassy filter ->
                                    responses
                                    |> List.filter (fun x -> x.Request.Embassy = filter.Embassy)
                                    |> paginate
                                    <| filter.Pagination
                                | Response.ByUserEmbassy filter ->
                                    responses
                                    |> List.filter (fun x ->
                                        x.Request.User.Id = filter.User.Id && x.Request.Embassy = filter.Embassy)
                                    |> paginate
                                    <| filter.Pagination

                            getData<External.Response> context key
                            |> Result.bind (Seq.map Internal.toResponse >> Dsl.Seq.roe)
                            |> Result.map filter
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Query.Response.get"))
                }

            let get' context responseId ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->

                            getData<External.Response> context key
                            |> Result.bind (Seq.map Internal.toResponse >> Dsl.Seq.roe)
                            |> Result.map (fun responses -> responses |> List.tryFind (fun x -> x.Id = responseId))
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Query.Response.get'"))
                }

    module Command =

        let private save<'a> context key (data: 'a array) =
            if data.Length = 1 then
                data
                |> Json.serialize
                |> Result.bind (fun value -> InMemory.add context key value)
            else
                data
                |> Json.serialize
                |> Result.bind (fun value -> InMemory.update context key value)

        module Request =

            let private add (requests: External.Request array) (request: Internal.Request) =
                match requests |> Array.tryFind (fun x -> x.Id = request.Id.Value) with
                | Some _ -> Error(Persistence $"Request {request.Id} already exists.")
                | _ -> Ok(requests |> Array.append [| External.toRequest request |])

            let private update (requests: External.Request array) (request: Internal.Request) =
                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                | None -> Error(Persistence $"Request {request.Id} not found to update.")
                | Some index ->
                    Ok(
                        requests
                        |> Array.mapi (fun i x -> if i = index then External.toRequest request else x)
                    )

            let private delete (requests: External.Request array) (request: Internal.Request) =
                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                | None -> Error(Persistence $"Request {request.Id} not found to delete.")
                | Some index -> Ok(requests |> Array.removeAt index)

            let execute context command ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let key = "requests"

                            getData<External.Request> context key
                            |> Result.bind (fun requests ->
                                match command with
                                | Command.Request.Add request -> add requests request
                                | Command.Request.Update request -> update requests request
                                | Command.Request.Delete request -> delete requests request)
                            |> Result.bind (fun requests -> save context key requests)
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Command.Request.execute"))
                }

        module Response =

            let private add (responses: External.Response array) (response: Internal.Response) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ -> Error(Persistence $"Response {response.Id} already exists.")
                | _ -> Ok(responses |> Array.append [| External.toResponse response |])

            let private update (responses: External.Response array) (response: Internal.Response) =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None -> Error(Persistence $"Response {response.Id} not found to update.")
                | Some index ->
                    Ok(
                        responses
                        |> Array.mapi (fun i x -> if i = index then External.toResponse response else x)
                    )

            let private delete (responses: External.Response array) (response: Internal.Response) =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None -> Error(Persistence $"Response {response.Id} not found to delete.")
                | Some index -> Ok(responses |> Array.removeAt index)

            let execute context command ct =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let key = "responses"

                            getData<External.Response> context key
                            |> Result.bind (fun responses ->
                                match command with
                                | Command.Response.Add response -> add responses response
                                | Command.Response.Update response -> update responses response
                                | Command.Response.Delete response -> delete responses response)
                            |> Result.bind (fun responses -> save context key responses)
                            |> Result.mapError Infrastructure

                        | _ -> Error(Logical(Cancelled "Command.Response.execute"))
                }

module Repository =

    ///<summary>Creates a storage context</summary>
    /// <param name="storage">The storage type</param>
    /// <returns>The storage context</returns>
    /// <remarks>Default is InMemory</remarks>
    let createStorage =
        function
        | Some storage -> Ok storage
        | _ -> Persistence.Core.createStorage InMemory

    module Query =

        module Request =

            let get storage filter ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Request.get context filter ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

            let get' storage requestId ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Request.get' context requestId ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

        module Response =

            let get storage filter ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Response.get context filter ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

            let get' storage responseId ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Response.get' context responseId ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

    module Command =

        module Request =

            let execute storage command ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Request.execute context command ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

            let add storage request ct =
                execute storage (Command.Request.Add request) ct

            let update storage request ct =
                execute storage (Command.Request.Update request) ct

            let delete storage request ct =
                execute storage (Command.Request.Delete request) ct

        module Response =

            let execute storage command ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Response.execute context command ct
                | _ -> async { return Error(Logical(NotSupported $"Storage {storage}")) }

            let add storage response ct =
                execute storage (Command.Response.Add response) ct

            let update storage response ct =
                execute storage (Command.Response.Update response) ct

            let delete storage response ct =
                execute storage (Command.Response.Delete response) ct
