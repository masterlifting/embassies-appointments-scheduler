module internal Eas.Persistence

open System.Threading
open Infrastructure.Domain.Errors
open Infrastructure.DSL.Threading
open Persistence.Core
open Domain.Internal.Core

module private InMemoryRepository =
    open Persistence.InMemory
    open Infrastructure.DSL.SerDe

    module Get =

        let initGetCountryCredentials (storage: Storage) =
            fun (city: Country) (ct: CancellationToken) ->
                async {
                    let key = $"{city}"

                    let deserialize =
                        Option.map <| Domain.Internal.Russian.createCredentials
                        >> Option.defaultValue (Error <| Persistence $"Not found credentials for {city}")

                    return
                        match ct |> notCanceled with
                        | true -> get storage key |> Result.bind deserialize
                        | _ -> Error <| Persistence "Operation canceled initGetCountryCredentials"
                }

        let initGetUserCredentials (storage: Storage) =
            fun (user: User) (city: Country) (ct: CancellationToken) ->
                async {
                    let key = $"{city}-{user.Name}"

                    return
                        match ct |> notCanceled with
                        | true -> get storage key
                        | _ -> Error <| Persistence "Operation canceled initGetUserCredentials"
                }

    module Set =
        let initSetCredentials (storage: Storage) =
            fun (user: User) (city: Country) (credentials: string) (ct: CancellationToken) ->
                async {
                    let key = $"{city}-{user.Name}"

                    return
                        match ct |> notCanceled with
                        | true -> add storage key credentials
                        | _ -> Error <| Persistence "Operation canceled initSetCredentials"
                }


module Repository =

    let getStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> createStorage InMemory

    module Russian =

        let initSetCredentials storage =
            fun user country credentials ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Set.initSetCredentials storage user country credentials ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initGetUserCredentials storage =
            fun user city ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetUserCredentials storage user city ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

        let initGetCountryCredentials storage =
            fun city ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetCountryCredentials storage city ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }
