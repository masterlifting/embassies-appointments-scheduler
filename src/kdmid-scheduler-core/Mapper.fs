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
        | "belgrad" -> Ok Core.Belgrade
        | "budapest" -> Ok Core.Budapest
        | "sarajevo" -> Ok Core.Sarajevo
        | _ -> Error "Invalid city code"

    let toCity =
        function
        | ToCity city -> city

module UserCredentials =
    let (|ToPersistence|) (input: Core.UserCredentials) : Persistence.UserCredential seq =
        input
        |> Seq.map (fun user cityCredentials ->
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
        input
        |> Seq.map (fun x ->
            x.Credentials
            |> Seq.map (fun y ->
                y.City
                |> KdmidCredentials.toCity
                |> Result.bind (fun city ->
                    y.Credentials
                    |> List.map KdmidCredentials.toCore
                    |> Infrastructure.DSL.Seq.resultOrError
                    |> Result.bind (fun kdmidCredentials -> Ok(city, set kdmidCredentials))))
            |> Infrastructure.DSL.Seq.resultOrError
            |> Result.bind (fun cityCredentials -> Ok (x.User |> User.toCore, cityCredentials)))
        |> Infrastructure.DSL.Seq.resultOrError
        |> Result.bind (fun userCredentials -> 
            userCredentials
            |> List.map (fun (user, cityCredentials) -> (user, cityCredentials |> List.map id |> Map.ofList))
            |> Map.ofList
            |> Ok)

    let toCore =
        function
        | ToCore userCredentials -> userCredentials
