module KdmidScheduler.Repository

open KdmidScheduler.Domain.Core
open Infrastructure
open Persistence

let addUserCredentials pScope city credentials =
    async {
        return
            match pScope with
            | Core.Scope.InMemoryStorageScope storage ->
                match DSL.SerDe.Json.serialize (Mapper.toPersistenceUserCredentials credentials) with
                | Ok serializedCredentials ->
                    let cityCode = Mapper.getCityCode city

                    match InMemoryStorage.add cityCode serializedCredentials storage with
                    | Ok _ -> Ok()
                    | Error error -> Error error
                | Error error -> Error error
            | _ -> Error $"Not implemented for '{pScope}'"
    }

let getUserCredentials pScope city =
    async {
        return
            match pScope with
            | Core.Scope.InMemoryStorageScope storage ->
                let cityCode = Mapper.getCityCode city

                match InMemoryStorage.find cityCode storage with
                | Error error -> Error error
                | Ok None -> Error $"User credentials for '{city}' are not found"
                | Ok(Some unserializedCredentials) ->
                    match DSL.SerDe.Json.deserialize<Domain.Persistence.UserCredential seq> unserializedCredentials with
                    | Ok credentials -> Ok(credentials |> Mapper.toCoreUserCredentials)
                    | Error error -> Error error
            | _ -> Error $"Not implemented for '{pScope}'"
    }

let getCityCredentials (user: User) : Async<Result<CityCredentials, string>> =
    async { return Error "Not implemented" }

let getKdmidCredentials (user: User) (city: City) : Async<Result<Set<Kdmid.Credentials>, string>> =
    async { return Error "Not implemented" }
