[<RequireQualifiedAccess>]
module internal EmbassyAccess.Persistence.InMemoryRepository

open Infrastructure
open Persistence.Domain
open EmbassyAccess.Domain
open EmbassyAccess.Mapper
open Persistence.InMemory

let private getEntities<'a> key context =
    context
    |> Storage.Query.get key
    |> Result.bind (Json.deserialize<'a array> |> Option.map >> Option.defaultValue (Ok [||]))

module Query =
    let paginate<'a> (data: 'a list) (pagination: Filter.Pagination<'a> option) =
        match pagination with
        | None -> data
        | Some pagination ->
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
                                filter.Ids = []
                                || filter.Ids |> List.contains x.Id
                                   && filter.Embassy
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
                                filter.Ids = []
                                || filter.Ids |> List.contains x.Id
                                   && filter.Request
                                      |> Option.map (fun request -> x.Request.Id = request.Id)
                                      |> Option.defaultValue true
                                   && filter.Modified
                                      |> Option.map (fun predicate -> predicate x.Modified)
                                      |> Option.defaultValue true)
                            |> paginate
                            <| filter.Pagination

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
                                filter.Ids = []
                                || filter.Ids |> List.contains x.Id
                                   && filter.Request
                                      |> Option.map (fun request -> x.Request.Id = request.Id)
                                      |> Option.defaultValue true
                                   && filter.Modified
                                      |> Option.map (fun predicate -> predicate x.Modified)
                                      |> Option.defaultValue true)
                            |> paginate
                            <| filter.Pagination

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

        let private update (response: AppointmentsResponse) (responses: External.AppointmentsResponse array) =
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

        let private delete (response: AppointmentsResponse) (responses: External.AppointmentsResponse array) =
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

        let private update (response: ConfirmationResponse) (responses: External.ConfirmationResponse array) =
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

        let private delete (response: ConfirmationResponse) (responses: External.ConfirmationResponse array) =
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
