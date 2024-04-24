module KdmidScheduler.Domain

module Core =

    module Kdmid =
        type KdmidId = Id of string
        type KdmidCd = Cd of string
        type KdmidEms = Ems of string option

        type Credentials =
            { Id: int
              Cd: string
              Ems: string option }

        let createCredentials id cd ems =
            match id, cd, ems with
            | Id id, Cd cd, Ems ems ->
                match id with
                | Infrastructure.DSL.AP.IsInt id ->
                    match cd with
                    | Infrastructure.DSL.AP.IsLettersOrNumbers cd ->
                        match ems with
                        | None -> { Id = id; Cd = cd; Ems = None } |> Ok
                        | Some ems ->
                            match ems with
                            | Infrastructure.DSL.AP.IsLettersOrNumbers ems -> { Id = id; Cd = cd; Ems = Some ems } |> Ok
                            | _ -> Error "Invalid EMS credential."
                    | _ -> Error "Invalid CD credential."
                | _ -> Error "Invalid ID credential."

        let (|Deconstruct|_|) credentials =
            match credentials with
            | { Id = id; Cd = cd; Ems = ems } -> Some(id, cd, ems)

    type City =
        | Belgrade
        | Budapest
        | Sarajevo

    type UserId = UserId of string
    type User = { Id: UserId; Name: string }


    type UserCredentials = Map<User, Set<Kdmid.Credentials>>
    type CityCredentials = Map<City, Set<Kdmid.Credentials>>

    type UserOrder =
        { User: User
          CityCredentials: CityCredentials }

    type CityOrder =
        { City: City
          UserCredentials: UserCredentials }

    type UserCityOrder =
        { User: User
          City: City
          Credentials: Set<Kdmid.Credentials> }

    type OrderResult =
        { Date: System.DateOnly
          Time: System.TimeOnly
          Description: string }

    type CityOrderResult = Map<User, Set<OrderResult>>
    type UserOrderResult = Map<City, Set<OrderResult>>

module Persistence =

    module Kdmid =
        type Credentials = { Id: int; Cd: string; Ems: string }

    type User = { Id: string; Name: string }

    type UserCredential =
        { User: User
          Credentials: Kdmid.Credentials list }
