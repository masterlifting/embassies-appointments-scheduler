module internal Eas.Persistence

open Infrastructure
open Infrastructure.Dsl.SerDe
open Infrastructure.Dsl.Threading
open Infrastructure.Domain.Errors
open Persistence.Domain
open Persistence.Storage
open Domain
open Mapper

module private InMemoryRepository =

    let private getExternal<'a> context key =
        context
        |> InMemory.get
        |> fun get -> get key
        |> Result.bind (Json.deserialize<'a array> |> Option.map >> Option.defaultValue (Ok [||]))

    module Query =

        module Request =

            let private getRequests context key =
                getExternal<External.Request> context key
                |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)

            let getByEmbassy context embassy ct =
                async {
                    let key = $"{embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getRequests context key
                            |> Result.map (List.filter (fun x -> x.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

            let getByUserEmbassy context (user: Internal.User) embassy ct =
                async {
                    let key = $"{embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getRequests context key
                            |> Result.map (List.filter (fun x -> x.User.Id = user.Id && x.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

        module Response =

            let private getResponses context key =
                getExternal<External.Response> context key
                |> Result.bind (Seq.map Internal.toResponse >> Dsl.Seq.roe)

            let getByEmbassy context embassy ct =
                async {
                    let key = $"{embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getResponses context key
                            |> Result.map (List.filter (fun x -> x.Request.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

            let getByUserEmbassy context (user: Internal.User) embassy ct =
                async {
                    let key = $"{embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getResponses context key
                            |> Result.map (
                                List.filter (fun x -> x.Request.Embassy = embassy && x.Request.User.Id = user.Id)
                            )
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

    module Command =

        module Request =

            let add context (request: Internal.Request) ct =
                async {
                    let key = $"{request.Embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Request> context key
                            |> Result.bind (fun requests ->
                                if requests.Length = 0 then
                                    [ External.toRequest request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value -> InMemory.add context key value)
                                else
                                    match requests |> Array.tryFind (fun x -> x.Id = request.Id.Value) with
                                    | Some _ -> Error <| Persistence $"Request {request.Id} already exists."
                                    | _ ->
                                        [ External.toRequest request ]
                                        |> Json.serialize
                                        |> Result.bind (fun value ->
                                            context |> InMemory.update |> (fun update -> update key value)))

                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

            let update context (request: Internal.Request) ct =
                async {
                    let key = $"{request.Embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Request> context key
                            |> Result.bind (fun requests ->
                                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                                | None -> Error <| Persistence $"Request {request.Id} not found to update."
                                | Some index ->
                                    requests
                                    |> Array.mapi (fun i x -> if i = index then External.toRequest request else x)
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

            let delete context (request: Internal.Request) ct =
                async {
                    let key = $"{request.Embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Request> context key
                            |> Result.bind (fun requests ->
                                match requests |> Array.tryFindIndex (fun x -> x.Id = request.Id.Value) with
                                | None -> Error <| Persistence $"Request {request.Id} not found to delete."
                                | Some index ->
                                    requests
                                    |> Array.removeAt index
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        module Response =

            let add context (response: Internal.Response) ct =
                async {
                    let key = $"{response.Request.Embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Response> context key
                            |> Result.bind (fun responses ->
                                match
                                    responses |> Array.tryFind (fun x -> x.Request.Id = response.Request.Id.Value)
                                with
                                | Some _ -> Error <| Persistence $"Response {response.Request.Id} already exists."
                                | _ ->
                                    [ External.toResponse response ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))

                        | _ -> Error <| Persistence "Operation canceled initSetResponse"
                }

            let update context (response: Internal.Response) ct =
                async {
                    let key = $"{response.Request.Embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Response> context key
                            |> Result.bind (fun responses ->
                                match
                                    responses
                                    |> Array.tryFindIndex (fun x -> x.Request.Id = response.Request.Id.Value)
                                with
                                | None -> Error <| Persistence $"Response {response.Request.Id} not found to update."
                                | Some index ->
                                    responses
                                    |> Array.mapi (fun i x -> if i = index then External.toResponse response else x)
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetResponse"
                }

            let delete context (response: Internal.Response) ct =
                async {
                    let key = $"{response.Request.Embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Response> context key
                            |> Result.bind (fun responses ->
                                match
                                    responses
                                    |> Array.tryFindIndex (fun x -> x.Request.Id = response.Request.Id.Value)
                                with
                                | None -> Error <| Persistence $"Response {response.Request.Id} not found to delete."
                                | Some index ->
                                    responses
                                    |> Array.removeAt index
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetResponse"
                }

module Repository =

    ///<summary>Creates a storage context</summary>
    /// <param name="storage">The storage type</param>
    /// <returns>The storage context</returns>
    /// <remarks>Default is InMemory</remarks>
    let createStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> Persistence.Core.createStorage InMemory

    module Query =

        module Request =

            let getByUserEmbassy storage user embassy ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Request.getByUserEmbassy context user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

            let getByEmbassy storage embassy ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Request.getByEmbassy context embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }

        module Response =

            let getByUserEmbassy storage user embassy ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Response.getByUserEmbassy context user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

            let getByEmbassy storage embassy ct =
                function
                | InMemoryContext context -> InMemoryRepository.Query.Response.getByEmbassy context embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }

    module Command =

        module Reuest =

            let add storage request ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Request.add context request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

            let update storage response ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Request.update context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

            let delete storage request ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Request.delete context request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        module Response =

            let add storage response ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Response.add context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

            let update storage response ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Response.update context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

            let delete storage response ct =
                function
                | InMemoryContext context -> InMemoryRepository.Command.Response.delete context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }
