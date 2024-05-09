module KdmidScheduler.Domain

open System

module Core =
    module User =
        type Id = UserId of string
        type User = { Id: Id; Name: string }

    module Kdmid =
        module private SupportedCities =
            [<Literal>]
            let Belgrade = "belgrad"

            [<Literal>]
            let Budapest = "budapest"

            [<Literal>]
            let Sarajevo = "sarajevo"

        type Id = private KdmidId of int
        type Cd = private KdmidCd of string
        type Ems = private KdmidEms of string option

        type City =
            | Belgrade
            | Budapest
            | Sarajevo

        type Credentials =
            { City: City
              Id: Id
              Cd: Cd
              Ems: Ems }

            member this.deconstruct() =
                match this with
                | { City = city
                    Id = KdmidId id
                    Cd = KdmidCd cd
                    Ems = KdmidEms ems } ->
                    match city with
                    | Belgrade -> (SupportedCities.Belgrade, id, cd, ems)
                    | Budapest -> (SupportedCities.Budapest, id, cd, ems)
                    | Sarajevo -> (SupportedCities.Sarajevo, id, cd, ems)

        type PublicCity = PublicCity of string
        type PublicId = PublicId of int
        type PublicCd = PublicCd of string
        type PublicEms = PublicEms of string option

        let createCredentials city id cd ems =

            let city' =
                match city with
                | PublicCity SupportedCities.Belgrade -> Ok Belgrade
                | PublicCity SupportedCities.Budapest -> Ok Budapest
                | PublicCity SupportedCities.Sarajevo -> Ok Sarajevo
                | _ -> Error "Invalid KDMID.City is not recognized."

            let id' =
                match id with
                | PublicId id when id > 1000 -> Ok(KdmidId id)
                | _ -> Error "Invalid KDMID.ID credential."

            let cd' =
                match cd with
                | PublicCd cd ->
                    match cd with
                    | Infrastructure.DSL.AP.IsLettersOrNumbers cd -> Ok(KdmidCd cd)
                    | _ -> Error "Invalid KDMID.CD credential."

            let ems' =
                match ems with
                | PublicEms ems ->
                    match ems with
                    | None -> Ok(KdmidEms(None))
                    | Some ems ->
                        match ems with
                        | Infrastructure.DSL.AP.IsString ems -> Ok(KdmidEms(Some ems))
                        | Infrastructure.DSL.AP.IsLettersOrNumbers ems -> Ok(KdmidEms(Some ems))
                        | _ -> Error "Invalid KDMID.EMS credential."

            city'
            |> Result.bind (fun city ->
                id'
                |> Result.bind (fun id ->
                    cd'
                    |> Result.bind (fun cd ->
                        ems'
                        |> Result.map (fun ems ->
                            { City = city
                              Id = id
                              Cd = cd
                              Ems = ems }))))

        type Result =
            { Date: DateOnly
              Time: TimeOnly
              Description: string }

        type Order = Credentials Set

        type OrderResult =
            { Credentials: Credentials
              Results: Result Set }

        type Error =
            | InvalidCredentials of string
            | InvalidResponse of string
            | InvalidRequest of string

    open User
    open Kdmid

    type UserKdmidOrder = { User: User; Order: Order }

    type UserKdmidOrderResult =
        { User: User; OrderResult: OrderResult }

module Persistence =

    module User =
        type User = { Id: string; Name: string }

    module Kdmid =
        type Credentials =
            { City: string
              Id: int
              Cd: string
              Ems: string }

        type Result =
            { Date: DateTime
              Time: DateTime
              Description: string }

        type Order = Credentials seq

        type OrderResult =
            { Credentials: Credentials
              Results: Result seq }

    open User
    open Kdmid

    type UserKdmidOrder =
        { User: User
          Credentials: Credentials seq }

    type UserKdmidOrderResult =
        { User: User; OrderResult: OrderResult }
