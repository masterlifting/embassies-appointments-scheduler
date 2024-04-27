module KdmidScheduler.Core

open KdmidScheduler.Domain.Core
open KdmidScheduler.Domain.Core.Kdmid

let private createUrlParams credentials =
    match credentials with
    | Deconstruct(id, cd, None) -> $"id={id}&cd={cd}"
    | Deconstruct(id, cd, Some ems) -> $"id={id}&cd={cd}&ems={ems}"

let private createBaseUrl city =
    let cityCode = city |> Mapper.KdmidCredentials.toCityCode
    $"https://{cityCode}.kdmid.ru/queue/"

let addUserCredentials = Repository.UserCredentials.add
let getUserCredentials = Repository.UserCredentials.get

let processCityOrder (order: CityOrder) storage : Async<Result<CityOrderResult seq, string>> =
    async { return Error "Not implemented" }

let processUserOrder (order: UserOrder) : Async<Result<UserOrderResult, string>> =
    async { return Error "Not implemented" }

let processOrder (order: UserCityOrder) : Async<Result<Set<OrderResult>, string>> =
    async { return Error "Not implemented" }
