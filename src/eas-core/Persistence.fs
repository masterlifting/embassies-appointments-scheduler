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

    let getMemoryStorage () = getStorage InMemory

    module Russian =

        open Domain.Internal.Russian
        open Web.Domain.Internal.Bots.Telegram

        let setCredentials
            (credentials: Credentials)
            (storage: Storage)
            (ct: CancellationToken)
            : Async<Result<unit, InfrastructureError>> =
            async { return Error <| Persistence "Not implemented" }

        let getCredentials
            (city: City)
            (storage: Storage)
            (ct: CancellationToken)
            : Async<Result<Set<Credentials> option, InfrastructureError>> =
            async { return Error <| Persistence "Not implemented" }

        let getAppointments
            (city: City)
            (storage: Storage)
            (ct: CancellationToken)
            : Async<Result<Set<Appointment> option, InfrastructureError>> =
            async { return Error <| Persistence "Not implemented" }

        let setAppointments
            (city: City)
            (appointments: Appointment seq)
            (storage: Storage)
            (ct: CancellationToken)
            : Async<Result<unit, InfrastructureError>> =
            async { return Error <| Persistence "Not implemented" }

        let getTelegramSubscribers
            (city: City)
            (storage: Storage)
            (ct: CancellationToken)
            : Async<Result<Set<ChatId> option, InfrastructureError>> =
            async { return Error <| Persistence "Not implemented" }
