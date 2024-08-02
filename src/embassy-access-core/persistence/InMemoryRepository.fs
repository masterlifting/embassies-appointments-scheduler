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
