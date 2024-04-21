module KdmidScheduler.Domain

module Core =

    module Kdmid =
        type Id = private Id of int
        type Cd = private Cd of string
        type Ems = private Ems of string option

        type Credentials = { Id: Id; Cd: Cd; Ems: Ems }

        let createCredentials (id: string) (cd: string) (ems: string option) =
            match id with
            | Infrastructure.DSL.AP.IsInt id ->
                match cd with
                | Infrastructure.DSL.AP.IsLettersOrNumbers cd ->
                    match ems with
                    | None ->
                        Ok
                            { Id = Id id
                              Cd = Cd cd
                              Ems = Ems None }
                    | Some ems ->
                        match ems with
                        | Infrastructure.DSL.AP.IsLettersOrNumbers ems ->
                            Ok
                                { Id = Id id
                                  Cd = Cd cd
                                  Ems = Ems(Some ems) }
                        | _ -> Error "Invalid EMS"
                | _ -> Error "Invalid CD"
            | _ -> Error "Invalid ID"

        let (|Deconstruct|_|) credentials =
            match credentials with
            | { Id = Id id
                Cd = Cd cd
                Ems = Ems ems } -> Some(id, cd, ems)

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

    type User = { Id: string; Name: string }

    type KdmidCredentials = { Id: string; Cd: string; Ems: string }

    type UserKdmidOrders =
        { User: User
          Orders: Map<string, Set<KdmidCredentials>> }
