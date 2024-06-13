module internal Eas.Persistence

open System.Threading
open Infrastructure
open Infrastructure.Dsl.SerDe
open Infrastructure.Dsl.Threading
open Infrastructure.Domain.Errors
open Persistence.Domain
open Persistence.Storage
open Domain
open Mapper

module private InMemoryRepository =

    let private getEmbassyRequests filter =
        let deserialize =
            Json.deserialize<External.Request array>
            |> Option.map
            >> Option.defaultValue (Ok [||])

        let map requests =
            match requests with
            | null -> []
            | [||] -> []
            | _ -> List.ofArray requests

        let filter requests = requests |> List.filter filter

        Result.bind (deserialize >> Result.map (map >> filter))

    module Get =

        let initGetEmbassyRequests (context: InMemory.Context) =
            fun (embassy: Internal.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            let embassy = External.toEmbassy embassy

                            context
                            |> InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.Embassy.Name = embassy.Name
                                && x.Embassy.Country.Name = embassy.Country.Name
                                && x.Embassy.Country.City.Name = embassy.Country.City.Name)
                            |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

        let initGetUserEmbassyRequests (context: InMemory.Context) =
            fun (user: Internal.User) (embassy: Internal.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            let embassy = External.toEmbassy embassy

                            context
                            |> InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.User.Name = user.Name
                                && x.Embassy.Name = embassy.Name
                                && x.Embassy.Country.Name = embassy.Country.Name
                                && x.Embassy.Country.City.Name = embassy.Country.City.Name)
                            |> Result.bind (Seq.map Internal.toRequest >> Dsl.Seq.roe)
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

    module Set =

        let initAddUserEmbassyRequest (context: InMemory.Context) =
            fun (user: Internal.User) (request: Internal.Request) (ct: CancellationToken) ->
                async {
                    let key = $"request-{request.Embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->

                            let request = External.toRequest user request

                            context
                            |> InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.User.Name = user.Name
                                && x.Embassy.Name = request.Embassy.Name
                                && x.Embassy.Country.Name = request.Embassy.Country.Name
                                && x.Embassy.Country.City.Name = request.Embassy.Country.City.Name)
                            |> Result.bind (fun requests ->
                                match requests |> List.find (fun x -> x.Id = request.Id) with
                                | true ->
                                    Error <| Persistence "Data to add already exists."
                                | _ ->
                                    [ External.toRequest user request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value -> context |> InMemory.add |> (fun add -> add key value)))

                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        let initUpdateUserEmbassyRequest (context: InMemory.Context) =
            fun (user: Internal.User) (request: Internal.Request) (ct: CancellationToken) ->
                async {
                    let key = $"request-{request.Embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->

                            let requestId =
                                request.Id
                                |> function
                                    | Internal.RequestId id -> id

                            context
                            |> InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x -> x.Id = requestId)
                            |> Result.bind (fun requests ->
                                match requests with
                                | requests when requests.Length <> 1 ->
                                    Error <| Persistence "Data to update is inconsistent."
                                | _ ->
                                    requests
                                    |> List.append [ External.toRequest user request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        let initDeleteUserEmbassyRequest (context: InMemory.Context) =
            fun (user: Internal.User) (request: Internal.Request) (ct: CancellationToken) ->
                async {
                    let key = $"request-{request.Embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            let request = External.toRequest user request

                            context
                            |> InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.User.Name = user.Name
                                && x.Embassy.Name = request.Embassy.Name
                                && x.Embassy.Country.Name = request.Embassy.Country.Name
                                && x.Embassy.Country.City.Name = request.Embassy.Country.City.Name)
                            |> Result.bind (fun requests ->

                                match requests with
                                | [] ->
                                    [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value -> context |> InMemory.add |> (fun add -> add key value))
                                | _ ->
                                    requests
                                    |> List.append [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        context |> InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        let initSetEmbassyResponse (context: InMemory.Context) =
            fun (response: Internal.Response) (ct: CancellationToken) ->
                async {
                    let key = $"response-{response}"

                    return Error <| Persistence "Not implemented initSetEmbassyResponse"
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

    module Set =

        let initSetUserEmbassyRequest storage =
            fun user request ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initSetUserEmbassyRequest context user request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initSetEmbassyResponse storage =
            fun response ct ->
                match storage with
                | InMemoryContext context -> InMemoryRepository.Set.initSetEmbassyResponse context response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }
