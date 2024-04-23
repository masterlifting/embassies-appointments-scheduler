module KdmidScheduler.Mapper

open Domain

let getCityCode city =
    match city with
    | Core.Belgrade -> "belgrad"
    | Core.Budapest -> "budapest"
    | Core.Sarajevo -> "sarajevo"

let toPersistenceKdmidCredentials (credentials: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials =
    { Id = credentials.Id
      Cd = credentials.Cd
      Ems =
        match credentials.Ems with
        | Some ems -> ems
        | None -> System.String.Empty }

let toCoreKdmidCredentials (credentials: Persistence.Kdmid.Credentials) : Core.Kdmid.Credentials =
    { Id = credentials.Id
      Cd = credentials.Cd
      Ems =
        match credentials.Ems with
        | "" -> None
        | ems -> Some ems }

let toPersistenceUser (user: Core.User) : Persistence.User =
    { Id =
        match user.Id with
        | Core.UserId id -> id
      Name = user.Name }

let toCoreUser (user: Persistence.User) : Core.User =
    { Id = Core.UserId user.Id
      Name = user.Name }

let toPersistenceUserCredentials (userCredentials: Core.UserCredentials) : Persistence.UserCredential seq =
    seq {
        for (user, credentials) in Map.toSeq userCredentials do
            let persistenceUserCredentials: Persistence.UserCredential =
                { User = toPersistenceUser user
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
