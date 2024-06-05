module internal Eas.Persistence

//module private InMemoryRepository =
//    open Persistence
//    open KdmidScheduler.SerDe

//    module User =

//        [<Literal>]
//        let UserKey = "user_"

//        let createKdmidOrder order storage =
//            match Json.User.serializeKdmidOrder order with
//            | Error error -> Error error
//            | Ok credentialsStr ->
//                let key = UserKey + (order.User.Id |> string)

//                match InMemory.add key credentialsStr storage with
//                | Error error -> Error error
//                | Ok _ -> Ok()


//        let getKdmidOrders city storage =
//            let key = UserKey + (city |> KdmidCredentials.toCityCode)

//            match InMemory.get key storage with
//            | Error error -> Error error
//            | Ok getResult ->
//                match getResult with
//                | Some credentialsStr ->
//                    match Json.User.deserialize credentialsStr with
//                    | Error error -> Error error
//                    | Ok credentials -> Ok <| Some credentials
//                | None -> Ok <| None

module Repository =
    open System.Threading
    open Infrastructure.Domain.Errors
    open Persistence.Core
    open Domain.Internal.Core

    let getStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> getStorage InMemory

    module Russian =

        let createSetCredentials (storage: Storage) =
            fun (user: User) (country: Country) (credentials: string) (ct: CancellationToken) ->
                async { return Error <| Persistence "Not implemented" }

        let createGetUserCredentials (storage: Storage) =
            fun (user: User) (city: Country) (ct: CancellationToken) ->
                async { return Error <| Persistence "Not implemented" }

        let createGetCountryCredentials (storage: Storage) =
            fun (city: Country) (ct: CancellationToken) -> async { return Error <| Persistence "Not implemented" }
