module internal KdmidScheduler.SerDe

//module Json =
//    open Infrastructure.DSL.SerDe.Json
//    open Domain
//    open Mapper

//    module User =
//        let serializeKdmidOrder = UserKdmidOrders.toPersistence >> serialize

//        let deserialize credentials =
//            match deserialize<Persistence.UserKdmdidOrder seq> credentials with
//            | Error error -> Error error
//            | Ok credentials ->
//                match UserKdmidOrders.toCore credentials with
//                | Error error -> Error error
//                | Ok credentials -> Ok credentials
