module KdmidScheduler.Domain

module Core =
    module Kdmid =
        type KdmidId = private KdmidId of int
        type KdmidCd = private KdmidCd of string
        type KdmidEms = private KdmidEms of string option

        type Credentials =
            { Id: KdmidId
              Cd: KdmidCd
              Ems: KdmidEms }

        let createCredentials id cd ems =
            match id with
            | id when id > 0 ->
                match cd with
                | Infrastructure.DSL.AP.IsLettersOrNumbers cd ->
                    match ems with
                    | None ->
                        { Id = KdmidId id
                          Cd = KdmidCd cd
                          Ems = KdmidEms None }
                        |> Ok
                    | Some ems ->
                        match ems with
                        | Infrastructure.DSL.AP.IsString ems ->
                            { Id = KdmidId id
                              Cd = KdmidCd cd
                              Ems = KdmidEms(Some ems) }
                            |> Ok
                        | Infrastructure.DSL.AP.IsLettersOrNumbers ems ->
                            { Id = KdmidId id
                              Cd = KdmidCd cd
                              Ems = KdmidEms(Some ems) }
                            |> Ok
                        | _ -> Error "Invalid EMS credential."
                | _ -> Error "Invalid CD credential."
            | _ -> Error "Invalid ID credential."

        let (|Deconstruct|) credentials =
            match credentials with
            | { Id = KdmidId id
                Cd = KdmidCd cd
                Ems = KdmidEms ems } -> (id, cd, ems)

    type City =
        | Belgrade
        | Budapest
        | Sarajevo

    type UserId = UserId of string
    type User = { Id: UserId; Name: string }

    type CityCredentials = Map<City, Set<Kdmid.Credentials>>
    type UserCredentials = Map<User, CityCredentials>

    type OrderResult =
        { Date: System.DateOnly
          Time: System.TimeOnly
          Description: string }

    type CityOrderResult = Map<City, Set<OrderResult>>

module Persistence =
    module Kdmid =
        type Credentials = { Id: int; Cd: string; Ems: string }

    type User = { Id: string; Name: string }

    type CityCredentials =
        { City: string
          Credentials: Kdmid.Credentials list }

    type UserCredential =
        { User: User
          Credentials: CityCredentials list }
