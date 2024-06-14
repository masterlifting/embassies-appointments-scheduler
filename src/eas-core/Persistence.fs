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

    module Get =

        let private getRequests context key =
            getExternal<External.Request> context key
            |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)

        let private getResponses context key =
            getExternal<External.Response> context key
            |> Result.bind (Seq.map Internal.toResponse >> Dsl.Seq.roe)

        let initGetEmbassyRequests context =
            fun embassy ct ->
                async {
                    let key = $"{embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getRequests context key
                            |> Result.map (List.filter (fun x -> x.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

        let initGetUserEmbassyRequests context =
            fun (user: Internal.User) embassy ct ->
                async {
                    let key = $"{embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getRequests context key
                            |> Result.map (List.filter (fun x -> x.User.Id = user.Id && x.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

        let initGetEmbassyResponses context =
            fun embassy ct ->
                async {
                    let key = $"{embassy}-responses"

                    return
                        match ct |> notCanceled with
                        | true ->
                            getResponses context key
                            |> Result.map (List.filter (fun x -> x.Request.Embassy = embassy))
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

        let initGetUserEmbassyResponses context =
            fun (user: Internal.User) embassy ct ->
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

    module Set =

        let initAddEmbassyRequest context =
            fun (request: Internal.Request) ct ->
                async {
                    let key = $"{request.Embassy}-requests"

                    return
                        match ct |> notCanceled with
                        | true ->

                            getExternal<External.Request> context key
                            |> Result.bind (fun requests ->
                                match requests |> Array.tryFind (fun x -> x.Id = request.Id.Value) with
                                | Some _ -> Error <| Persistence $"Request {request.Id} already exists."
                                | _ ->
                                    [ External.toRequest request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))

                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        let initUpdateEmbassyRequest context =
            fun (request: Internal.Request) ct ->
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

        let initDeleteEmbassyRequest context =
            fun (request: Internal.Request) ct ->
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

        let initAddEmbassyResponse context =
            fun (response: Internal.Response) ct ->
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

        let initUpdateEmbassyResponse context =
            fun (response: Internal.Response) ct ->
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

        let initDeleteEmbassyResponse context =
            fun (response: Internal.Response) ct ->
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

    module Get =

        let initGetUserEmbassyRequests storage =
            fun user embassy ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Get.initGetUserEmbassyRequests context user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

        let initGetEmbassyRequests storage =
            fun embassy ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Get.initGetEmbassyRequests context embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }

        let initGetUserEmbassyResponses storage =
            fun user embassy ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Get.initGetUserEmbassyResponses context user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

        let initGetEmbassyResponses storage =
            fun embassy ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Get.initGetEmbassyResponses context embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }

    module Set =

        let initAddEmbassyRequest storage =
            fun request ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initAddEmbassyRequest context request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initUpdateEmbassyRequest storage =
            fun response ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initUpdateEmbassyRequest context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

        let initDeleteEmbassyRequest storage =
            fun request ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initDeleteEmbassyRequest context request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initAddEmbassyResponse storage =
            fun response ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initAddEmbassyResponse context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

        let initUpdateEmbassyResponse storage =
            fun response ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initUpdateEmbassyResponse context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }

        let initDeleteEmbassyResponse storage =
            fun response ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initDeleteEmbassyResponse context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }
