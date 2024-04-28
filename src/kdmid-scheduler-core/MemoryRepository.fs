module internal KdmidScheduler.MemoryRepository

open Persistence
open KdmidScheduler.SerDe
open KdmidScheduler.Mapper

module User =
    let add user storage =
        match Json.User.serialize user with
        | Error error -> Error error
        | Ok userStr ->
            let key = user.Id |> string

            match InMemory.add key userStr storage with
            | Ok _ -> Ok()
            | Error error -> Error error

    let get userId storage =
        let key = userId |> string

        match InMemory.get key storage with
        | Ok getResult ->
            match getResult with
            | Some userStr -> Ok <| (Some <| Json.User.deserialize userStr)
            | None -> Ok <| None
        | Error error -> Error error

module UserCredentials =
    let add city credentials storage =
        match Json.UserCredentials.serialize credentials with
        | Error error -> Error error
        | Ok credentialsStr ->
            let key = city |> KdmidCredentials.toCityCode

            match InMemory.add key credentialsStr storage with
            | Error error -> Error error
            | Ok _ -> Ok()

    let get city storage =
        let key = city |> KdmidCredentials.toCityCode

        match InMemory.get key storage with
        | Error error -> Error error
        | Ok getResult ->
            match getResult with
            | Some credentialsStr ->
                match Json.UserCredentials.deserialize credentialsStr with
                | Error error -> Error error
                | Ok credentials -> Ok <| Some credentials
            | None -> Ok <| None
