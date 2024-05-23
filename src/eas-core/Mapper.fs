module Eas.Mapper

//open System
//open Domain

//module User =
//    let (|ToPersistence|) (input: Core.User.User) : Persistence.User.User =
//        match input with
//        | { Id = Core.User.UserId id
//            Name = name } -> { Id = id; Name = name }

//    let toPersistence =
//        function
//        | ToPersistence user -> user

//    let (|ToCore|) (input: Persistence.User.User) : Core.User.User =
//        match input with
//        | { Id = id; Name = name } ->
//            { Id = Core.User.UserId id
//              Name = name }

//    let toCore =
//        function
//        | ToCore user -> user

//module Kdmid =
//    module Credentials =
//        let (|ToPersistence|) (input: Core.Kdmid.Credentials) : Persistence.Kdmid.Credentials =
//            let city, id, cd, ems = input.Deconstructed()
//            match ems with
//            | None -> { City= city; Id = id; Cd = cd; Ems = String.Empty }
//            | Some ems -> { City= city; Id = id; Cd = cd; Ems = ems }

//        let toPersistence =
//            function
//            | ToPersistence credentials -> credentials

//        let (|ToCore|) (input: Persistence.Kdmid.Credentials) : Result<Core.Kdmid.Credentials, string> =
//            match input with
//            | {City = city; Id = id; Cd = cd; Ems = ems } ->
//                let city' = Core.Kdmid.PublicCity city
//                let id' = Core.Kdmid.PublicKdmidId id
//                let cd' = Core.Kdmid.PublicKdmidCd cd
//                let ems' = if ems = String.Empty then Core.Kdmid.PublicKdmidEms None else Core.Kdmid.PublicKdmidEms (Some ems)
//                Core.Kdmid.createCredentials city' id' cd' ems'

//        let toCore =
//            function
//            | ToCore credentials -> credentials

//module UserKdmidOrders =
//    let (|ToPersistence|) (input: Core.KdmidOrder) : Persistence.UserKdmdidOrder seq =
//        input
//        |> Seq.map (fun coreUserCredentials ->
//            { User = coreUserCredentials.Key |> User.toPersistence
//              Credentials =
//                coreUserCredentials.Value
//                |> Seq.map (fun coreCityCredentials ->
//                    let persistenceCityCredentials: Persistence.CityCredentials =
//                        { City = coreCityCredentials.Key |> KdmidCredentials.toCityCode
//                          Credentials =
//                            coreCityCredentials.Value
//                            |> Set.map KdmidCredentials.toPersistence
//                            |> Seq.toList }

//                    persistenceCityCredentials)
//                |> Seq.toList })

//    let toPersistence =
//        function
//        | ToPersistence userCredentials -> userCredentials

//    let (|ToCore|) (input: Persistence.UserKdmdidOrder seq) : Result<Core.KdmidOrder, string> =
//        input
//        |> Seq.map (fun x ->
//            x.Credentials
//            |> Seq.map (fun y ->
//                y.City
//                |> KdmidCredentials.toCity
//                |> Result.bind (fun city ->
//                    y.Credentials
//                    |> List.map UserKdmid.toCore
//                    |> Infrastructure.DSL.Seq.resultOrError
//                    |> Result.bind (fun kdmidCredentials -> Ok(city, set kdmidCredentials))))
//            |> Infrastructure.DSL.Seq.resultOrError
//            |> Result.bind (fun cityCredentials ->
//                let user = x.User |> User.toCore
//                Ok(user, cityCredentials)))
//        |> Infrastructure.DSL.Seq.resultOrError
//        |> Result.bind (fun userCredentials ->
//            userCredentials
//            |> List.map (fun (user, cityCredentials) -> (user, cityCredentials |> List.map id |> Map.ofList))
//            |> Map.ofList
//            |> Ok)

//    let toCore =
//        function
//        | ToCore userCredentials -> userCredentials
