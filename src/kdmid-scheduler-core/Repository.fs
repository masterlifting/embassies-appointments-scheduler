module KdmidScheduler.Repository

open KdmidScheduler.Domain.Core

let getUserCredentials (city: City) : Async<Result<UserCredentials, string>> =
    async { return Error "Not implemented" }

let getCityCredentials (user: User) : Async<Result<CityCredentials, string>> =
    async { return Error "Not implemented" }

let getKdmidCredentials (user: User) (city: City) : Async<Result<Set<Kdmid.Credentials>, string>> =
    async { return Error "Not implemented" }
