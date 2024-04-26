module KdmidScheduler.Mapper

open Domain

module AP =
    let (|Map|) (input: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials option =
        match input with
        | Core.Kdmid.Deconstruct(id, cd, None) ->
            Some
                { Id = id
                  Cd = cd
                  Ems = System.String.Empty }
        | Core.Kdmid.Deconstruct(id, cd, Some ems) -> Some { Id = id; Cd = cd; Ems = ems }
        | _ -> None

    let (|Map|_|) (input: Persistence.Kdmid.Credentials) : Core.Kdmid.Credentials option =
        match input with
        | { Id = id; Cd = cd; Ems = ems } ->
            match Domain.Core.Kdmid.createCredentials id cd (Some ems) with
            | Ok credentials -> Some credentials
            | Error _ -> None

    let (|ToPersistenceUser|) (input: Core.User) : Persistence.User=
        match input with
        | { Id = Core.UserId id; Name = name } -> { Id = id; Name = name }

    let (|ToCoreUser|) (input: Persistence.User) : Core.User =
        match input with
        | { Id = id; Name = name } -> { Id = Core.UserId id; Name = name }

let getCityCode city =
    match city with
    | Core.Belgrade -> "belgrad"
    | Core.Budapest -> "budapest"
    | Core.Sarajevo -> "sarajevo"

let toPersistenceUserCredentials (userCredentials: Core.UserCredentials) : Persistence.UserCredential seq =
    seq {
        for (user, credentials) in Map.toSeq userCredentials do
            let persistenceUserCredentials: Persistence.UserCredential =
                { User = match user with
                         | AP.ToPersistenceUser(user) -> user
                  Credentials = Set.map toPersistenceKdmidCredentials credentials |> Set.toList }

            yield persistenceUserCredentials
    }

let toCoreUserCredentials (userCredentials: Persistence.UserCredential seq) : Core.UserCredentials =
    let userCredentialsMap =
        userCredentials
        |> Seq.map (fun userCredential ->
            let user = toCoreUser userCredential.User

            let credentials =
                userCredential.Credentials |> List.map toCoreKdmidCredentials |> Set.ofList

            user, credentials)
        |> Map.ofSeq

    userCredentialsMap
