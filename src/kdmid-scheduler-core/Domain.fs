module KdmidScheduler.Domain

module Core =

    module Kdmid =
        type Id = Id of string
        type Cd = Cd of string
        type Ems = Ems of string option

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
                        | None -> Ok { Id = id; Cd = cd; Ems = None }
                        | Some ems ->
                            match ems with
                            | Infrastructure.DSL.AP.IsLettersOrNumbers ems -> Ok { Id = id; Cd = cd; Ems = Some ems }
                            | _ -> Error "Invalid EMS"
                    | _ -> Error "Invalid CD"
                | _ -> Error "Invalid ID"

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

    type OrderResult =
        { Date: System.DateOnly
          Time: System.TimeOnly
          Description: string }

    type CityOrderResult = Map<User, Set<OrderResult>>
    type UserOrderResult = Map<City, Set<OrderResult>>

module Persistence =

    type User = { Id: string; Name: string }

    type KdmidCredentials = { Id: string; Cd: string; Ems: string }

    type UserKdmidOrders =
        { User: User
          Orders: Map<string, Set<KdmidCredentials>> }
