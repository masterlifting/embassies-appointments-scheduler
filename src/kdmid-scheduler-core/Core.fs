module KdmidScheduler.Core

open KdmidScheduler.Domain.Core
open KdmidScheduler.Domain.Core.Kdmid

let createUrlParams credentials =
    match credentials with
    | Deconstruct(id, cd, None) -> $"id={id}&cd={cd}"
    | Deconstruct(id, cd, Some ems) -> $"id={id}&cd={cd}&ems={ems}"
    | _ -> ""

let private getCityCode city =
    match city with
    | Belgrade -> "belgrad"
    | Budapest -> "budapest"
    | Sarajevo -> "sarajevo"

let createBaseUrl city =
    let cityCode = getCityCode city
    $"https://{cityCode}.kdmid.ru/queue/"

let addUserCredentials pScope city =
    async {
        match createCredentials (Id "1") (Cd "2") (Ems(Some "3")) with
        | Error error -> return Error error
        | Ok credential ->
            let user: Domain.Core.User = { Id = UserId "1"; Name = "John Doe" }

            let userCredentials: Domain.Core.UserCredentials = Map [ user, Set [ credential ] ]

            match! Repository.addUserCredentials pScope city userCredentials with
            | Error error -> return Error error
            | Ok _ -> return Ok "User credentials were added"
    }

let getCityCredentials (user: User) : Async<Result<CityCredentials, string>> =
    async {
        let! credentials = Repository.getCityCredentials user
        return credentials
    }

let getKdmidCredentials (user: User) (city: City) : Async<Result<Set<Kdmid.Credentials>, string>> =
    async {
        let! credentials = Repository.getKdmidCredentials user city
        return credentials
    }

let processCityOrder pScope (order: CityOrder) : Async<Result<CityOrderResult seq, string>> =
    async {
        // let cityCode = Kdmid.createUrlParams order.UserCredentials.[order.City].Head
        // let baseUrl = Kdmid.createBaseUrl order.City
        // let url = $"{baseUrl}?{cityCode}"
        // let! availableDates = Infrastructure.Http.getAvailableDates url
        // return availableDates

        return Error "Not implemented"
    }

let processUserOrder (order: UserOrder) : Async<Result<UserOrderResult, string>> =
    async {
        // let cityCode = Kdmid.createUrlParams order.CityCredentials.[order.User].Head
        // let baseUrl = Kdmid.createBaseUrl order.City
        // let url = $"{baseUrl}?{cityCode}"
        // let! availableDates = Infrastructure.Http.getAvailableDates url
        // return availableDates

        return Error "Not implemented"
    }

let processOrder (order: UserCityOrder) : Async<Result<Set<OrderResult>, string>> =
    async {
        // let cityCode = Kdmid.createUrlParams order.UserCredentials.[city].Head
        // let baseUrl = Kdmid.createBaseUrl city
        // let url = $"{baseUrl}?{cityCode}"
        // let! availableDates = Infrastructure.Http.getAvailableDates url
        // return availableDates

        return Error "Not implemented"
    }
