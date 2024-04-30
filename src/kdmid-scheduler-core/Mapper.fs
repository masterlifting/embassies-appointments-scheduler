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
        | Core.Kdmid.Deconstruct(id, cd, Some ems) -> { Id = id; Cd = cd; Ems = ems }
        | Core.Kdmid.Deconstruct(id, cd, None) -> { Id = id; Cd = cd; Ems = String.Empty }

    let toPersistence =
        function
        | ToPersistence credentials -> credentials

    let (|ToCore|) (input: Persistence.Kdmid.Credentials) : Result<Core.Kdmid.Credentials, string> =
        match input with
        | { Id = id; Cd = cd; Ems = ems } -> Core.Kdmid.createCredentials id cd (Some ems)

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

    let (|ToCity|) cityCode =
        match cityCode with
        | "belgrad" -> Some Core.Belgrade
        | "budapest" -> Some Core.Budapest
        | "sarajevo" -> Some Core.Sarajevo
        | _ -> None

    let toCity =
        function
        | ToCity city -> city

module UserCredentials =
    let (|ToPersistence|) (input: Core.UserCredentials) : Persistence.UserCredential seq =
        input
        |> Map.toSeq
        |> Seq.map (fun (user, cityCredentials) ->
            { User = User.toPersistence user
              Credentials =
                cityCredentials
                |> Map.map (fun city credentials ->
                    let credentials: Persistence.CityCredentials =
                        { City = KdmidCredentials.toCityCode city
                          Credentials = credentials |> List.map KdmidCredentials.toPersistence }

                    credentials) })

    let toPersistence =
        function
        | ToPersistence userCredentials -> userCredentials

    let (|ToCore|) (input: Persistence.UserCredential seq) : Result<Core.UserCredentials, string> =
        let result =
            input
            |> Seq.map (fun inputItem ->
                let user = inputItem.User |> User.toCore

                let cityCredentials =
                    inputItem.Credentials
                    |> List.map (fun cityCredentialsItem ->
                        match cityCredentialsItem.City |> KdmidCredentials.toCity with
                        | None -> Error "Invalid city code"
                        | Some city ->
                            let kdmidCredentialsRes =
                                cityCredentialsItem.Credentials
                                |> List.map KdmidCredentials.toCore
                                |> Infrastructure.DSL.Seq.resultOrError

                            Result.bind (fun kdmidCredentials -> Ok(city, set kdmidCredentials)) kdmidCredentialsRes)
                    |> Infrastructure.DSL.Seq.resultOrError

                Result.bind (fun credentials -> Ok(user, credentials)) cityCredentials)
            |> Infrastructure.DSL.Seq.resultOrError

        let toUserCredentials userCredentials =
            userCredentials
            |> List.map (fun (user, cityCredentials) -> (user, cityCredentials |> List.map id |> Map.ofList))
            |> Map.ofList

        Result.bind (fun userCredentials -> Ok(toUserCredentials userCredentials)) result

    let toCore =
        function
        | ToCore userCredentials -> userCredentials
