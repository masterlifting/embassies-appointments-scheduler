module KdmidScheduler.Domain

module Core =
    type City =
        | Belgrade
        | Budapest
        | Sarajevo

    type UserId = UserId of string
    type User = { Id: UserId; Name: string }

    module Kdmid =
        type Id = Id of string
        type Cd = Cd of string
        type Ems = Ems of string option

        type Id' = private Id' of int
        type Cd' = private Cd' of string
        type Ems' = private Ems' of string option

        type Credentials = { Id: Id'; Cd: Cd'; Ems: Ems' }

        let createCredentials id cd ems =
            match id, cd, ems with
            | Id id, Cd cd, Ems ems ->
                match id with
                | Infrastructure.DSL.AP.IsInt id ->
                    match cd with
                    | Infrastructure.DSL.AP.IsLettersOrNumbers cd ->
                        match ems with
                        | None ->
                            Ok
                                { Id = Id' id
                                  Cd = Cd' cd
                                  Ems = Ems' None }
                        | Some ems ->
                            match ems with
                            | Infrastructure.DSL.AP.IsLettersOrNumbers ems ->
                                Ok
                                    { Id = Id' id
                                      Cd = Cd' cd
                                      Ems = Ems'(Some ems) }
                            | _ -> Error "Invalid EMS"
                    | _ -> Error "Invalid CD"
                | _ -> Error "Invalid ID"

        let createUrlParams credentials =
            match credentials with
            | { Id = Id' id
                Cd = Cd' cd
                Ems = Ems' ems } ->
                match ems with
                | Some ems -> $"id={id}&cd={cd}&ems={ems}"
                | None -> $"id={id}&cd={cd}"

        let private getCityCode city =
            match city with
            | Belgrade -> "belgrad"
            | Budapest -> "budapest"
            | Sarajevo -> "sarajevo"

        let createBaseUrl city =
            let cityCode = getCityCode city
            $"https://{cityCode}.kdmid.ru/queue/"


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
