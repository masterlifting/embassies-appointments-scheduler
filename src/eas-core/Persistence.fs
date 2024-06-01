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
    open Infrastructure.Domain.Errors

    module Russian =
        open Domain.Core.Russian

        let getCredentials city ct : Async<Result<Credentials Set option, InfrastructureError>> =
            async { return Ok <| Some [] }
//    module User =
//        open Persistence.Core
//        open Infrastructure.Logging
//        open Domain.Core.User
//        open Domain.Core.Kdmid

//        let createKdmidOrder order storage =
//            async {
//                return
//                    match storage with
//                    | Storage.MemoryStorage storage -> InMemoryRepository.User.createKdmidOrder order storage
//                    | _ -> Error $"Not implemented for '{storage}'."
//            }

//        let getKdmidOrders city storage =
//            async {
//                return
//                    match storage with
//                    | Storage.MemoryStorage mStorage -> InMemoryRepository.User.getKdmidOrders city mStorage
//                    | _ -> Error $"Not implemented for '{storage}'."
//            }

//        let createTestKdmidOrder city =
//            async {
//                match Storage.create Type.InMemory with
//                | Error error -> error |> Log.error
//                | Ok storage ->

//                    let user =
//                        { Id = UserId "1"
//                          Name = "John"
//                          Type = Regular }

//                    let kdmidCredentials =
//                        [| 1; 2 |]
//                        |> Seq.map (fun x ->
//                            let city = PublicCity city
//                            let id = PublicKdmidId x
//                            let cd = PublicKdmidCd(x |> string)
//                            let ems = PublicKdmidEms None
//                            createCredentials city id cd ems)
//                        |> Infrastructure.DSL.Seq.resultOrError

//                    match kdmidCredentials with
//                    | Error error -> error |> Log.error
//                    | Ok credentials ->

//                        let order = { User = user; Order = set credentials }

//                        match! createKdmidOrder order storage with
//                        | Error error -> error |> Log.error
//                        | Ok _ -> $"User order was added.\n%A{order}" |> Log.info
//            }
