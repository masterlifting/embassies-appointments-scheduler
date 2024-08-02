module EmbassyAccess.Persistence

open Infrastructure
open Persistence.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Mapper

[<RequireQualifiedAccess>]
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
        { Pagination: Pagination<Domain.Request>
          Embassy: Embassy option
          Modified: Predicate<DateTime> option }

    type AppointmentsResponse =
        { Pagination: Pagination<AppointmentsResponse>
          Request: Domain.Request option
          Modified: Predicate<DateTime> option }

    type ConfirmationResponse =
        { Pagination: Pagination<ConfirmationResponse>
          Request: Domain.Request option
          Modified: Predicate<DateTime> option }

module private Command =

    type Request =
        | Create of Domain.Request
        | Update of Domain.Request
        | Delete of Domain.Request

    type AppointmentsResponse =
        | Create of Domain.AppointmentsResponse
        | Update of Domain.AppointmentsResponse
        | Delete of Domain.AppointmentsResponse

    type ConfirmationResponse =
        | Create of Domain.ConfirmationResponse
        | Update of Domain.ConfirmationResponse
        | Delete of Domain.ConfirmationResponse

module private InMemoryRepository =
    open Persistence.InMemory

    let private getEntities<'a> key context =
        context
        |> Storage.Query.get key
        |> Result.bind (Json.deserialize<'a array> |> Option.map >> Option.defaultValue (Ok [||]))

    module Query =
        let paginate<'a> (data: 'a list) (pagination: Filter.Pagination<'a>) =
            data
            |> match pagination.SortBy with
               | Filter.Asc sortBy ->
                   match sortBy with
                   | Filter.Date getValue -> List.sortBy <| getValue
                   | Filter.String getValue -> List.sortBy <| getValue
                   | Filter.Int getValue -> List.sortBy <| getValue
                   | Filter.Bool getValue -> List.sortBy <| getValue
                   | Filter.Guid getValue -> List.sortBy <| getValue
               | Filter.Desc sortBy ->
                   match sortBy with
                   | Filter.Date getValue -> List.sortByDescending <| getValue
                   | Filter.String getValue -> List.sortByDescending <| getValue
                   | Filter.Int getValue -> List.sortByDescending <| getValue
                   | Filter.Bool getValue -> List.sortByDescending <| getValue
                   | Filter.Guid getValue -> List.sortByDescending <| getValue
            |> List.skip (pagination.PageSize * (pagination.Page - 1))
            |> List.truncate pagination.PageSize

        module Request =

            [<Literal>]
            let private key = "requests"

            let get ct (filter: Filter.Request) context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (requests: Request list) =
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
                            |> Result.bind (Seq.map toRequest >> DSL.Seq.roe)
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
                            |> Result.bind (Seq.map toRequest >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = requestId))
                        | _ -> Error <| Cancelled "Query.Request.get'"
                }

        module AppointmentsResponse =

            [<Literal>]
            let private key = "appointmentsResponses"

            let get ct (filter: Filter.AppointmentsResponse) context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: AppointmentsResponse list) =
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
                            |> Result.bind (Seq.map toAppointmentsResponse >> DSL.Seq.roe)
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
                            |> Result.bind (Seq.map toAppointmentsResponse >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = responseId))
                        | _ -> Error <| Cancelled "Query.AppointmentsResponse.get'"
                }

        module ConfirmationResponse =

            [<Literal>]
            let private key = "confirmationResponses"

            let get ct (filter: Filter.ConfirmationResponse) context =
                async {
                    return
                        match ct |> notCanceled with
                        | true ->
                            let filter (responses: ConfirmationResponse list) =
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
                            |> Result.bind (Seq.map toConfirmationResponse >> DSL.Seq.roe)
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
                            |> Result.bind (Seq.map toConfirmationResponse >> DSL.Seq.roe)
                            |> Result.map (List.tryFind (fun x -> x.Id = responseId))
                        | _ -> Error <| Cancelled "Query.ConfirmationResponse.get'"
                }

    module Command =

        let private save<'a> key context (data: 'a array) =
            if data.Length = 1 then
                data
                |> Json.serialize
                |> Result.bind (fun value -> context |> Storage.Command.add key value)
            else
                data
                |> Json.serialize
                |> Result.bind (fun value -> context |> Storage.Command.update key value)

        module Request =

            let private add (request: Request) (requests: External.Request array) =
                match requests |> Array.tryFind (fun x -> x.Id = request.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Request {request.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(requests |> Array.append [| External.toRequest request |])

            let private update (request: Request) (requests: External.Request array) =
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

            let private delete (request: Request) (requests: External.Request array) =
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

            let private add (response: AppointmentsResponse) (responses: External.AppointmentsResponse array) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(responses |> Array.append [| External.toAppointmentsResponse response |])

            let private update
                (response: AppointmentsResponse)
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
                (response: AppointmentsResponse)
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

            let private add (response: ConfirmationResponse) (responses: External.ConfirmationResponse array) =
                match responses |> Array.tryFind (fun x -> x.Id = response.Id.Value) with
                | Some _ ->
                    Error
                    <| Operation
                        { Message = $"Response {response.Id} already exists."
                          Code = Some ErrorCodes.AlreadyExists }
                | _ -> Ok(responses |> Array.append [| External.toConfirmationResponse response |])

            let private update
                (response: ConfirmationResponse)
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
                (response: ConfirmationResponse)
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
    open Persistence

    module Query =

        module Request =

            let get ct filter storage =
                match storage with
                | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.Request.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct requestId storage =
                match storage with
                | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.Request.get' ct requestId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        module AppointmentsResponse =

            let get ct filter storage =
                match storage with
                | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.AppointmentsResponse.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct responseId storage =
                match storage with
                | Storage.Type.InMemory context ->
                    context |> InMemoryRepository.Query.AppointmentsResponse.get' ct responseId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        module ConfirmationResponse =

            let get ct filter storage =
                match storage with
                | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.ConfirmationResponse.get ct filter
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let get' ct responseId storage =
                match storage with
                | Storage.Type.InMemory context ->
                    context |> InMemoryRepository.Query.ConfirmationResponse.get' ct responseId
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

    module Command =

        module Request =

            let private execute ct command storage =
                match storage with
                | Storage.Type.InMemory context -> context |> InMemoryRepository.Command.Request.execute ct command
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
                | Storage.Type.InMemory context ->
                    context |> InMemoryRepository.Command.AppointmentsResponse.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let save ct response =
                Command.AppointmentsResponse.Create response |> execute ct

            let update ct response =
                Command.AppointmentsResponse.Update response |> execute ct

            let delete ct response =
                Command.AppointmentsResponse.Delete response |> execute ct

        module ConfirmationResponse =

            let private execute ct command storage =
                match storage with
                | Storage.Type.InMemory context ->
                    context |> InMemoryRepository.Command.ConfirmationResponse.execute ct command
                | _ -> async { return Error <| NotSupported $"Storage {storage}" }

            let create ct response =
                Command.ConfirmationResponse.Create response |> execute ct

            let update ct response =
                Command.ConfirmationResponse.Update response |> execute ct

            let delete ct response =
                Command.ConfirmationResponse.Delete response |> execute ct
