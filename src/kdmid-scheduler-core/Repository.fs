module KdmidScheduler.Repository

open KdmidScheduler.Domain.Core
open Infrastructure

let addUserCredentials city credentials storage =
    async {
        return
            match storage with
            | Persistence.Core.Storage.MemoryStorage storage ->
                match DSL.SerDe.Json.serialize (Mapper.toPersistenceUserCredentials credentials) with
                | Ok serializedCredentials ->
                    let cityCode = Mapper.getCityCode city

                    match Persistence.InMemory.add cityCode serializedCredentials storage with
                    | Ok _ -> Ok()
                    | Error error -> Error error
                | Error error -> Error error
            | _ -> Error $"Not implemented for '{storage}'."
    }

let getUserCredentials city storage =
    async {
        return
            match storage with
            | Persistence.Core.Storage.MemoryStorage storage ->
                let cityCode = Mapper.getCityCode city

                match Persistence.InMemory.find cityCode storage with
                | Error error -> Error error
                | Ok None -> Error $"User credentials for '{city}' are not found."
                | Ok(Some unserializedCredentials) ->
                    match DSL.SerDe.Json.deserialize<Domain.Persistence.UserCredential seq> unserializedCredentials with
                    | Ok credentials -> Ok(credentials |> Mapper.toCoreUserCredentials)
                    | Error error -> Error error
            | _ -> Error $"Not implemented for '{storage}'."
    }

let getCityCredentials (user: User) : Async<Result<CityCredentials, string>> =
    async { return Error "Not implemented." }

let getKdmidCredentials (user: User) (city: City) : Async<Result<Set<Kdmid.Credentials>, string>> =
    async { return Error "Not implemented." }
