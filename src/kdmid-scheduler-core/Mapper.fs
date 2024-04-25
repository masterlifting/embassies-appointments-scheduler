module KdmidScheduler.Mapper

open Infrastructure.Logging
open Domain

let getCityCode city =
    match city with
    | Core.Belgrade -> "belgrad"
    | Core.Budapest -> "budapest"
    | Core.Sarajevo -> "sarajevo"

let toPersistenceKdmidCredentials (credentials: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials =
    match credentials with
    | Core.Kdmid.Deconstruct(id, cd, None) ->
        { Id = id
          Cd = cd
          Ems = System.String.Empty }
    | Core.Kdmid.Deconstruct(id, cd, Some ems) -> { Id = id; Cd = cd; Ems = ems }
    | _ ->
        { Id = 0
          Cd = System.String.Empty
          Ems = System.String.Empty }

let toCoreKdmidCredentials (credentials: Persistence.Kdmid.Credentials) : Core.Kdmid.Credentials =
    Core.Kdmid.createCredentials credentials.Id credentials.Cd (Some credentials.Ems)

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


module AP =

    let (|Map|_|) (input: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials option =
        match input with
        | Core.Kdmid.Deconstruct(id, cd, None) ->
            Some
                { Id = id
                  Cd = cd
                  Ems = System.String.Empty }
        | Core.Kdmid.Deconstruct(id, cd, Some ems) -> Some { Id = id; Cd = cd; Ems = ems }
        | _ ->
            Log.error "Invalid KDMID credentials."
            None

    let (|Map|_|) (input: Persistence.Kdmid.Credentials) : Core.Kdmid.Credentials option =
        match Core.Kdmid.createCredentials input.Id input.Cd (Some input.Ems) with
        | Ok credentials -> Some credentials
        | Error msg ->
            Log.error msg
            None

    let (|Map|_|) (input: Core.User) : Persistence.User option =
        match input with
        | Core.User(id, name) -> Some { Id = id; Name = name }
