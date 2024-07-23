module Eas.Persistence

open Infrastructure
open Infrastructure.DSL.SerDe
open Infrastructure.DSL.Threading
open Infrastructure.Domain.Errors
open Persistence.Domain.Core
open Persistence.Storage
open Domain
open Mapper

module Filter =
    open System

    type OrderBy<'a> =
        | Date of ('a -> DateTime)
        | String of ('a -> string)
        | Int of ('a -> int)
        | Bool of ('a -> bool)
        | Guid of ('a -> Guid)

    type SortBy<'a> =
        | Asc of OrderBy<'a>
        | Desc of OrderBy<'a>

    type Pagination<'a> =
        { Page: int
          PageSize: int
          SortBy: SortBy<'a> }

    type EmbassyFilter<'a> =
        { Pagination: Pagination<'a>
          Embassy: Internal.Embassy }

    type UserEmbassyFilter<'a> =
        { Pagination: Pagination<'a>
          Embassy: Internal.Embassy }

    type RequestFilter =
        { Pagination: Pagination<Internal.Request>
          Embassy: Internal.Embassy option
          Modified: DateTime option }

    type Request =
        | ByEmbassy of EmbassyFilter<Internal.Request>
        | ByUserEmbassy of UserEmbassyFilter<Internal.Request>

    type Response =
        | ByEmbassy of EmbassyFilter<Internal.AppointmentsResponse>
        | ByUserEmbassy of UserEmbassyFilter<Internal.AppointmentsResponse>

module private Command =

    type Request =
        | Create of Internal.Request
        | Update of Internal.Request
        | Delete of Internal.Request

    type Response =
        | Create of Internal.AppointmentsResponse
        | Update of Internal.AppointmentsResponse
        | Delete of Internal.AppointmentsResponse

module private InMemoryRepository =

    let private getEntities<'a> key context =
        context
        |> InMemory.Query.get key
        |> Result.bind (Json.deserialize<'a array> |> Option.map >> Option.defaultValue (Ok [||]))


    module Query =
        open Filter

        let paginate<'a> (data: 'a list) (pagination: Pagination<'a>) =
            data
            |> match pagination.SortBy with
               | Asc sortBy ->
                   match sortBy with
                   | Date getValue -> List.sortBy <| getValue
                   | String getValue -> List.sortBy <| getValue
                   | Int getValue -> List.sortBy <| getValue
                   | Bool getValue -> List.sortBy <| getValue
                   | Guid getValue -> List.sortBy <| getValue
               | Desc sortBy ->
                   match sortBy with
                   | Date getValue -> List.sortByDescending <| getValue
                   | String getValue -> List.sortByDescending <| getValue
                   | Int getValue -> List.sortByDescending <| getValue
                   | Bool getValue -> List.sortByDescending <| getValue
                   | Guid getValue -> List.sortByDescending <| getValue
            |> List.skip (pagination.PageSize * (pagination.Page - 1))
            |> List.truncate pagination.PageSize

        module Request =

            [<Literal>]
            let private key = "requests"

            let get ct filter context =
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
                                    requests |> List.filter (fun x -> x.Embassy = filter.Embassy) |> paginate
                                    <| filter.Pagination

                            context
                            |> getEntities<External.Request> key
                            |> Result.bind (Seq.map Internal.toRequest >> DSL.Seq.roe)
                            |> Result.map filter
                        | _ -> Error <| Cancelled "Query.Request.get"
                }

            let get' ct requestId context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            context
                            |> getEntities<External.Request> key
                            |> Result.bind (Seq.map Internal.toRequest >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = requestId))
                        | _ -> Error <| Cancelled "Query.Request.get'"
                }

        module Response =

            [<Literal>]
            let private key = "responses"

            let get ct filter context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: Internal.AppointmentsResponse list) =
                                match filter with
                                | Response.ByEmbassy filter ->
                                    responses
                                    |> List.filter (fun x -> x.Request.Embassy = filter.Embassy)
                                    |> paginate
                                    <| filter.Pagination
                                | Response.ByUserEmbassy filter ->
                                    responses
                                    |> List.filter (fun x -> x.Request.Embassy = filter.Embassy)
                                    |> paginate
                                    <| filter.Pagination

                            context
                            |> getEntities<External.Response> key
                            |> Result.bind (Seq.map Internal.toResponse >> DSL.Seq.roe)
                            |> Result.map filter
                        | _ -> Error <| Cancelled "Query.Response.get"
                }

            let get' ct responseId context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            context
                            |> getEntities<External.Response> key
                            |> Result.bind (Seq.map Internal.toResponse >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = responseId))
                        | _ -> Error <| Cancelled "Query.Response.get'"
                }

    module Command =

        let private save<'a> key context (data: 'a array) =
            if data.Length = 1 then
                data
                |> Json.serialize
                |> Result.bind (fun value -> context |> InMemory.Command.add key value)
            else
                data
                |> Json.serialize
                |> Result.bind (fun value -> context |> InMemory.Command.update key value)

        module Request =

            let private add (request: Internal.Request) (requests: External.Request array) =
                match requests |> Array.tryFind (fun x -> x.Id = request.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Request {request.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(requests |> Array.append [| External.toRequest request |])

            let private update (request: Internal.Request) (requests: External.Request array) =
                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Request {request.Id} not found to update."
                          Code = Some ErrorCodes.NotFound }
                | Some index ->
                    Ok(
                        requests
                        |> Array.mapi (fun i x -> if i = index then External.toRequest request else x)
                    )

            let private delete (request: Internal.Request) (requests: External.Request array) =
                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Request {request.Id} not found to delete."
                          Code = Some ErrorCodes.NotFound }
                | Some index -> Ok(requests |> Array.removeAt index)

            let execute ct command context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let key = "requests"

                            context
                            |> getEntities<External.Request> key
                            |> Result.bind (fun requests ->
                                match command with
                                | Command.Request.Create request -> requests |> add request
                                | Command.Request.Update request -> requests |> update request
                                | Command.Request.Delete request -> requests |> delete request)
                            |> Result.bind (context |> save key)
                        | _ -> Error <| Cancelled "Command.Request.execute"
                }

        module Response =

            let private add (response: Internal.AppointmentsResponse) (responses: External.Response array) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(responses |> Array.append [| External.toResponse response |])

            let private update (response: Internal.AppointmentsResponse) (responses: External.Response array) =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} not found to update."
                          Code = Some ErrorCodes.NotFound }
                | Some index ->
                    Ok(
                        responses
                        |> Array.mapi (fun i x -> if i = index then External.toResponse response else x)
                    )

            let private delete (response: Internal.AppointmentsResponse) (responses: External.Response array) =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} not found to delete."
                          Code = Some ErrorCodes.NotFound }
                | Some index -> Ok(responses |> Array.removeAt index)

            let execute ct command context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let key = "responses"

                            context
                            |> getEntities<External.Response> key
                            |> Result.bind (fun responses ->
                                match command with
                                | Command.Response.Create response -> responses |> add response
                                | Command.Response.Update response -> responses |> update response
                                | Command.Response.Delete response -> responses |> delete response)
                            |> Result.bind (context |> save key)
                        | _ -> Error <| Cancelled "Command.Response.execute"
                }

module Repository =
    open Persistence.Storage.Core


    ///<summary>Creates a storage context</summary>
    /// <param name="storage">The storage context</param>
    /// <returns>The storage context</returns>
    /// <remarks>Default is InMemory</remarks>
    let createStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> createStorage InMemory

    [<RequireQualifiedAccess>]
    module Query =

        module Request =

            let get ct filter storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.Request.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct requestId storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.Request.get' ct requestId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        module Response =

            let get ct filter storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.Response.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct responseId storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.Response.get' ct responseId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

    [<RequireQualifiedAccess>]
    module Command =

        module Request =

            let private execute ct command storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Command.Request.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let create ct request =
                Command.Request.Create request |> execute ct

            let update ct request =
                Command.Request.Update request |> execute ct

            let delete ct request =
                Command.Request.Delete request |> execute ct

        module Response =

            let private execute ct command storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Command.Response.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let create ct response =
                Command.Response.Create response |> execute ct

            let update ct response =
                Command.Response.Update response |> execute ct

            let delete ct response =
                Command.Response.Delete response |> execute ct
