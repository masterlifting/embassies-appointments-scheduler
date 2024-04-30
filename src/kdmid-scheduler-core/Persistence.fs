module internal KdmidScheduler.Persistence

module private InMemoryRepository =
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
        [<Literal>]
        let UserCredentialsKey = "user_credentials_"

        let add city credentials storage =
            match Json.UserCredentials.serialize credentials with
            | Error error -> Error error
            | Ok credentialsStr ->
                let key = UserCredentialsKey + (city |> KdmidCredentials.toCityCode)

                match InMemory.add key credentialsStr storage with
                | Error error -> Error error
                | Ok _ -> Ok()

        let get city storage =
            let key = UserCredentialsKey + (city |> KdmidCredentials.toCityCode)

            match InMemory.get key storage with
            | Error error -> Error error
            | Ok getResult ->
                match getResult with
                | Some credentialsStr ->
                    match Json.UserCredentials.deserialize credentialsStr with
                    | Error error -> Error error
                    | Ok credentials -> Ok <| Some credentials
                | None -> Ok <| None

module Repository =
    open Persistence.Core.Storage

    module User =
        let add user storage =
            async {
                return
                    match storage with
                    | MemoryStorage storage -> InMemoryRepository.User.add user storage
                    | _ -> Error $"Not implemented for '{storage}'."
            }

        let get id storage =
            async {
                return
                    match storage with
                    | MemoryStorage storage -> InMemoryRepository.User.get id storage
                    | _ -> Error $"Not implemented for '{storage}'."
            }

    module UserCredentials =
        open Infrastructure.Logging
        open KdmidScheduler.Domain.Core

        let add city credentials storage =
            async {
                return
                    match storage with
                    | MemoryStorage storage -> InMemoryRepository.UserCredentials.add city credentials storage
                    | _ -> Error $"Not implemented for '{storage}'."
            }

        let get city storage =
            async {
                return
                    match storage with
                    | MemoryStorage mStorage -> InMemoryRepository.UserCredentials.get city mStorage
                    | _ -> Error $"Not implemented for '{storage}'."
            }

        let createTest city =
            async {
                match create Persistence.Core.Type.InMemory with
                | Error error -> error |> Log.error
                | Ok storage ->

                    let user: User = { Id = UserId "1"; Name = "John" }

                    let kdmidCredentials =
                        [| 1; 2 |]
                        |> Seq.map (fun x -> Kdmid.createCredentials x (x |> string) None)
                        |> Infrastructure.DSL.Seq.resultOrError

                    match kdmidCredentials with
                    | Error error -> error |> Log.error
                    | Ok kdmidCredentials ->
                        let userCredentials: UserCredentials =
                            Map [ user, Map [ city, kdmidCredentials |> set ] ]

                        match! add city userCredentials storage with
                        | Error error -> error |> Log.error
                        | Ok _ -> $"User credentials added.\n{userCredentials}" |> Log.info
            }
