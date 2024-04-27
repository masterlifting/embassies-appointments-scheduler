module KdmidScheduler.Mapper

open System
open Domain

module User =
    let (|ToPersistence|) (input: Core.User) : Persistence.User =
        match input with
        | { Id = Core.UserId id; Name = name } -> { Id = id; Name = name }

    let toPersistence =
        function
        | ToPersistence user -> user

    let (|ToCore|) (input: Persistence.User) : Core.User =
        match input with
        | { Id = id; Name = name } -> { Id = Core.UserId id; Name = name }

    let toCore =
        function
        | ToCore user -> user

module KdmidCredentials =

    let (|ToPersistence|) (input: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials =
        match input with
        | Domain.Core.Kdmid.Deconstruct(id, cd, Some ems) -> { Id = id; Cd = cd; Ems = ems }
        | Domain.Core.Kdmid.Deconstruct(id, cd, None) -> { Id = id; Cd = cd; Ems = String.Empty }

    let toPersistence =
        function
        | ToPersistence credentials -> credentials

    let (|ToCore|) (input: Persistence.Kdmid.Credentials) : Result<Core.Kdmid.Credentials, string> =
        match input with
        | { Id = id; Cd = cd; Ems = ems } -> Domain.Core.Kdmid.createCredentials id cd (Some ems)

    let toCore =
        function
        | ToCore credentials -> credentials

    let (|ToCityCode|) city =
        match city with
        | Core.Belgrade -> "belgrad"
        | Core.Budapest -> "budapest"
        | Core.Sarajevo -> "sarajevo"

    let toCityCode =
        function
        | ToCityCode city -> city

module UserCredentials =
    let (|ToPersistence|) (input: Core.UserCredentials) : Persistence.UserCredential seq =
        input
        |> Map.toSeq
        |> Seq.map (fun (user, credentials) ->
            { User = User.toPersistence user
              Credentials = credentials |> Set.map KdmidCredentials.toPersistence |> Set.toList })

    let toPersistence =
        function
        | ToPersistence userCredentials -> userCredentials

    let (|ToCore|) (input: Persistence.UserCredential seq) : Result<Core.UserCredentials, string> =
        let result =
            input
            |> Seq.map (fun userCredentials ->
                let user = userCredentials.User |> User.toCore

                let credentials =
                    userCredentials.Credentials
                    |> List.map KdmidCredentials.toCore
                    |> List.toSeq
                    |> Infrastructure.DSL.Seq.resultOrError

                match credentials with
                | Error msg -> Error msg
                | Ok credentials -> Ok <| (user, credentials |> set))
            |> Infrastructure.DSL.Seq.resultOrError

        match result with
        | Error msg -> Error msg
        | Ok userCredentials -> Ok <| (userCredentials |> Map.ofList)

    let toCore =
        function
        | ToCore userCredentials -> userCredentials
