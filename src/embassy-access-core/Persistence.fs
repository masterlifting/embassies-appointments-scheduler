module EmbassyAccess.Persistence

open Infrastructure
open Persistence.Domain.Core
open Persistence.Storage
open EmbassyAccess.Domain
open EmbassyAccess.Mapper

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

    type Predicate<'a> = 'a -> bool

    type Pagination<'a> =
        { Page: int
          PageSize: int
          SortBy: SortBy<'a> }

    type Request =
        { Pagination: Pagination<Internal.Request>
          Embassy: Internal.Embassy option
          Modified: Predicate<DateTime> option }

    type AppointmentsResponse =
        { Pagination: Pagination<Internal.AppointmentsResponse>
          Request: Internal.Request option
          Modified: Predicate<DateTime> option }

    type ConfirmationResponse =
        { Pagination: Pagination<Internal.ConfirmationResponse>
          Request: Internal.Request option
          Modified: Predicate<DateTime> option }

module private Command =

    type Request =
        | Create of Internal.Request
        | Update of Internal.Request
        | Delete of Internal.Request

    type AppointmentsResponse =
        | Create of Internal.AppointmentsResponse
        | Update of Internal.AppointmentsResponse
        | Delete of Internal.AppointmentsResponse

    type ConfirmationResponse =
        | Create of Internal.ConfirmationResponse
        | Update of Internal.ConfirmationResponse
        | Delete of Internal.ConfirmationResponse

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
                                requests
                                |> List.filter (fun x ->
                                    filter.Embassy
                                    |> Option.map (fun embassy -> x.Embassy = embassy)
                                    |> Option.defaultValue true
                                    && filter.Modified
                                       |> Option.map (fun predicate -> predicate x.Modified)
                                       |> Option.defaultValue true)
                                |> paginate
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

        module AppointmentsResponse =

            [<Literal>]
            let private key = "appointmentsResponses"

            let get ct filter context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: Internal.AppointmentsResponse list) =
                                responses
                                |> List.filter (fun x ->
                                    filter.Request
                                    |> Option.map (fun request -> x.Request.Id = request.Id)
                                    |> Option.defaultValue true
                                    && filter.Modified
                                       |> Option.map (fun predicate -> predicate x.Modified)
                                       |> Option.defaultValue true)
                                |> paginate

                            context
                            |> getEntities<External.AppointmentsResponse> key
                            |> Result.bind (Seq.map Internal.toAppointmentsResponse >> DSL.Seq.roe)
                            |> Result.map filter
                        | _ -> Error <| Cancelled "Query.AppointmentsResponse.get"
                }

            let get' ct responseId context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            context
                            |> getEntities<External.AppointmentsResponse> key
                            |> Result.bind (Seq.map Internal.toAppointmentsResponse >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = responseId))
                        | _ -> Error <| Cancelled "Query.AppointmentsResponse.get'"
                }

        module ConfirmationResponse =

            [<Literal>]
            let private key = "confirmationResponses"

            let get ct filter context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: Internal.ConfirmationResponse list) =
                                responses
                                |> List.filter (fun x ->
                                    filter.Request
                                    |> Option.map (fun request -> x.Request.Id = request.Id)
                                    |> Option.defaultValue true
                                    && filter.Modified
                                       |> Option.map (fun predicate -> predicate x.Modified)
                                       |> Option.defaultValue true)
                                |> paginate

                            context
                            |> getEntities<External.ConfirmationResponse> key
                            |> Result.bind (Seq.map Internal.toConfirmationResponse >> DSL.Seq.roe)
                            |> Result.map filter
                        | _ -> Error <| Cancelled "Query.ConfirmationResponse.get"
                }

            let get' ct responseId context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            context
                            |> getEntities<External.ConfirmationResponse> key
                            |> Result.bind (Seq.map Internal.toConfirmationResponse >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = responseId))
                        | _ -> Error <| Cancelled "Query.ConfirmationResponse.get'"
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

        module AppointmentsResponse =

            let private add (response: Internal.AppointmentsResponse) (responses: External.AppointmentsResponse array) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(responses |> Array.append [| External.toAppointmentsResponse response |])

            let private update
                (response: Internal.AppointmentsResponse)
                (responses: External.AppointmentsResponse array)
                =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} not found to update."
                          Code = Some ErrorCodes.NotFound }
                | Some index ->
                    Ok(
                        responses
                        |> Array.mapi (fun i x ->
                            if i = index then
                                External.toAppointmentsResponse response
                            else
                                x)
                    )

            let private delete
                (response: Internal.AppointmentsResponse)
                (responses: External.AppointmentsResponse array)
                =
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
                            let key = "appointmentsResponses"

                            context
                            |> getEntities<External.AppointmentsResponse> key
                            |> Result.bind (fun responses ->
                                match command with
                                | Command.AppointmentsResponse.Create response -> responses |> add response
                                | Command.AppointmentsResponse.Update response -> responses |> update response
                                | Command.AppointmentsResponse.Delete response -> responses |> delete response)
                            |> Result.bind (context |> save key)
                        | _ -> Error <| Cancelled "Command.AppointmentsResponse.execute"
                }

        module ConfirmationResponse =

            let private add (response: Internal.ConfirmationResponse) (responses: External.ConfirmationResponse array) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(responses |> Array.append [| External.toConfirmationResponse response |])

            let private update
                (response: Internal.ConfirmationResponse)
                (responses: External.ConfirmationResponse array)
                =
                match responses |> Array.tryFindIndex (fun x -> x.Id = response.Id.Value) with
                | None ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} not found to update."
                          Code = Some ErrorCodes.NotFound }
                | Some index ->
                    Ok(
                        responses
                        |> Array.mapi (fun i x ->
                            if i = index then
                                External.toConfirmationResponse response
                            else
                                x)
                    )

            let private delete
                (response: Internal.ConfirmationResponse)
                (responses: External.ConfirmationResponse array)
                =
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
                            let key = "confirmationResponses"

                            context
                            |> getEntities<External.ConfirmationResponse> key
                            |> Result.bind (fun responses ->
                                match command with
                                | Command.ConfirmationResponse.Create response -> responses |> add response
                                | Command.ConfirmationResponse.Update response -> responses |> update response
                                | Command.ConfirmationResponse.Delete response -> responses |> delete response)
                            |> Result.bind (context |> save key)
                        | _ -> Error <| Cancelled "Command.ConfirmationResponse.execute"

                }

[<RequireQualifiedAccess>]
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

        module AppointmentsResponse =

            let get ct filter storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.AppointmentsResponse.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct responseId storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.AppointmentsResponse.get' ct responseId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        module ConfirmationResponse =

            let get ct filter storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.ConfirmationResponse.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct responseId storage =
                match storage with
                | InMemoryStorage context -> context |> InMemoryRepository.Query.ConfirmationResponse.get' ct responseId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

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

        module AppointmentsResponse =

            let private execute ct command storage =
                match storage with
                | InMemoryStorage context ->
                    context |> InMemoryRepository.Command.AppointmentsResponse.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let create ct response =
                Command.AppointmentsResponse.Create response |> execute ct

            let update ct response =
                Command.AppointmentsResponse.Update response |> execute ct

            let delete ct response =
                Command.AppointmentsResponse.Delete response |> execute ct

        module ConfirmationResponse =

            let private execute ct command storage =
                match storage with
                | InMemoryStorage context ->
                    context |> InMemoryRepository.Command.ConfirmationResponse.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let create ct response =
                Command.ConfirmationResponse.Create response |> execute ct

            let update ct response =
                Command.ConfirmationResponse.Update response |> execute ct

            let delete ct response =
                Command.ConfirmationResponse.Delete response |> execute ct
