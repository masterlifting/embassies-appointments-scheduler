module internal KdmidScheduler.SerDe

module Json =
    open Infrastructure.DSL.SerDe.Json
    open Domain
    open Mapper

    module User =
        let serialize = User.toPersistence >> serialize

        let deserialize user =
            match deserialize<Persistence.User> user with
            | Error error -> Error error
            | Ok user -> Ok <| User.toCore user

    module UserCredentials =
        let serialize = UserCredentials.toPersistence >> serialize

        let deserialize credentials =
            match deserialize<Persistence.UserCredential seq> credentials with
            | Error error -> Error error
            | Ok credentials ->
                match UserCredentials.toCore credentials with
                | Error error -> Error error
                | Ok credentials -> Ok credentials
