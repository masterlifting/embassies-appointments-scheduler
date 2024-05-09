module internal KdmidScheduler.Persistence

module private InMemoryRepository =
    open Persistence
    open KdmidScheduler.SerDe

    [<Literal>]
    let UserCredentialsKey = "user_credentials_"

    let addOrder order storage =
        match Json.UserKdmidOrders.serialize order with
        | Error error -> Error error
        | Ok credentialsStr ->
            let key = UserCredentialsKey + (city |> KdmidCredentials.toCityCode)

            match InMemory.add key credentialsStr storage with
            | Error error -> Error error
            | Ok _ -> Ok()

    let getOrdersByCity city storage =
        let key = UserCredentialsKey + (city |> KdmidCredentials.toCityCode)

        match InMemory.get key storage with
        | Error error -> Error error
        | Ok getResult ->
            match getResult with
            | Some credentialsStr ->
                match Json.UserKdmidOrders.deserialize credentialsStr with
                | Error error -> Error error
                | Ok credentials -> Ok <| Some credentials
            | None -> Ok <| None

module Repository =

    open Persistence.Core
    open Infrastructure.Logging
    open Domain.Core
    open Domain.Core.User
    open Domain.Core.Kdmid

    let addOrder order storage =
        async {
            return
                match storage with
                | Storage.MemoryStorage storage -> InMemoryRepository.addOrder order storage
                | _ -> Error $"Not implemented for '{storage}'."
        }

    let getOrdersByCity city storage =
        async {
            return
                match storage with
                | Storage.MemoryStorage mStorage -> InMemoryRepository.getOrdersByCity city mStorage
                | _ -> Error $"Not implemented for '{storage}'."
        }

    let createTestOrder city =
        async {
            match Storage.create Type.InMemory with
            | Error error -> error |> Log.error
            | Ok storage ->

                let user = { Id = UserId "1"; Name = "John" }

                let kdmidCredentials =
                    [| 1; 2 |]
                    |> Seq.map (fun x ->
                        let city = PublicCity city
                        let id = PublicId x
                        let cd = PublicCd(x |> string)
                        let ems = PublicEms None
                        createCredentials city id cd ems)
                    |> Infrastructure.DSL.Seq.resultOrError

                match kdmidCredentials with
                | Error error -> error |> Log.error
                | Ok credentials ->

                    let order = { User = user; Order = set credentials }

                    match! addOrder order storage with
                    | Error error -> error |> Log.error
                    | Ok _ -> $"User order was added.\n%A{order}" |> Log.info
        }
