module internal Eas.Persistence

open System.Threading
open Infrastructure
open Infrastructure.Domain.Errors
open Infrastructure.DSL.Threading
open Persistence.Core

module private InMemoryRepository =
    open Infrastructure.DSL.SerDe
    open Domain

    let private mapRequests filter =
        let deserialize =
            Option.map <| Json.deserialize<External.Request array>
            >> Option.defaultValue (Error <| Persistence $"Not found requests.")

        let map requests =
            match requests with
            | null -> []
            | [||] -> []
            | _ -> List.ofArray requests

        let filter requests = requests |> List.filter filter

        Result.bind (deserialize >> Result.map (map >> filter))

    module Get =

        let initGetEmbassyRequests (storage: Persistence.InMemory.Storage) =
            fun (embassy: Internal.Core.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            Persistence.InMemory.get storage key
                            |> mapRequests (fun x -> x.EmbassyId = embassy.Id)
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

        let initGetUserEmbassyRequests (storage: Persistence.InMemory.Storage) =
            fun (user: Internal.Core.User) (embassy: Internal.Core.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            Persistence.InMemory.get storage key
                            |> mapRequests (fun x -> x.EmbassyId = embassy.Id && x.UserId = user.Id)
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

    module Set =

        let initSetUserEmbassyRequest (storage: Persistence.InMemory.Storage) =
            fun (user: Internal.Core.User) (embassy: Internal.Core.Embassy) (data: string) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            Persistence.InMemory.get storage key
                            |> mapRequests (fun x -> x.EmbassyId = embassy.Id && x.UserId = user.Id)
                            |> Result.bind (fun requests ->

                                let request = new External.Request()
                                request.Id <- 1
                                request.Data <- data
                                request.EmbassyId <- embassy.Id
                                request.UserId <- user.Id

                                match requests with
                                | [] ->
                                    [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value -> Persistence.InMemory.add storage key value)
                                | _ ->
                                    requests
                                    |> List.append [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value -> Persistence.InMemory.update storage key value))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }


module Repository =

    let createStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> createStorage InMemory

    module Russian =

        let initSetCredentials storage =
            fun user embassy credentials ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Set.initSetUserEmbassyRequest storage user embassy credentials ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initGetUserCredentials storage =
            fun user embassy ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetUserEmbassyRequests storage user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

        let initGetCredentials storage =
            fun embassy ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetEmbassyRequests storage embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }
