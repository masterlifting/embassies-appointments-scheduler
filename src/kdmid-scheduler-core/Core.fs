module KdmidScheduler.Core

open Infrastructure.Logging
open KdmidScheduler.Domain.Core

let addUserCredentials = Repository.UserCredentials.add
let getUserCredentials = Repository.UserCredentials.get

let private checkCalendar city credentials =

    let rec innerLoop city credentials error =
        async {
            match credentials with
            | [] ->
                match error with
                | None -> return Ok None
                | Some error -> return Error error
            | credential :: credentialsTail ->
                match! Web.Http.getCalendar city credential with
                | Error error -> return! innerLoop city credentialsTail (Some error)
                | Ok None -> return Ok None
                | Ok(Some orderResults) -> return Ok <| Some orderResults
        }

    innerLoop city credentials None

let processCityOrder (order: CityOrder) storage : Async<Result<string, string>> =
    async {
        let credentials = order.UserCredentials.Values |> Seq.concat |> Seq.toList

        match! checkCalendar order.City credentials with
        | Error error -> return Error error
        | Ok None -> return Ok "No available dates."
        | Ok(Some orderResults) -> return Ok "Order processed."
    }

let processUserOrder (order: UserOrder) : Async<Result<UserOrderResult, string>> =
    async { return Error "processUserOrder not implemented." }

let processOrder (order: UserCityOrder) : Async<Result<Set<OrderResult>, string>> =
    async { return Error "processOrder not implemented." }

let createTestUserCredentials city =
    async {
        match Persistence.Core.Storage.create Persistence.Core.Type.InMemory with
        | Error error -> error |> Log.error
        | Ok storage ->

            let user: Domain.Core.User = { Id = UserId "1"; Name = "John" }

            let kdmidCredentials =
                [| 1; 2 |]
                |> Seq.map (fun x -> Domain.Core.Kdmid.createCredentials x (x |> string) None)
                |> Infrastructure.DSL.Seq.resultOrError

            match kdmidCredentials with
            | Error error -> error |> Log.error
            | Ok kdmidCredentials ->
                let userCredentials: UserCredentials = Map [ user, kdmidCredentials |> set ]

                match! addUserCredentials city userCredentials storage with
                | Error error -> error |> Log.error
                | Ok _ -> $"User credentials added.\n{userCredentials}" |> Log.info
    }
