module KdmidScheduler.Domain

open System

module Core =

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
            private
            | Belgrade
            | Budapest
            | Sarajevo

        type PublicCity = PublicCity of string
        type PublicId = PublicKdmidId of int
        type PublicCd = PublicKdmidCd of string
        type PublicEms = PublicKdmidEms of string option

        type Credentials =
            { City: City
              Id: Id
              Cd: Cd
              Ems: Ems }

            member this.Deconstructed =
                match this with
                | { City = city
                    Id = KdmidId id
                    Cd = KdmidCd cd
                    Ems = KdmidEms ems } ->
                    match city with
                    | Belgrade ->
                        (PublicCity SupportedCities.Belgrade, PublicKdmidId id, PublicKdmidCd cd, PublicKdmidEms ems)
                    | Budapest ->
                        (PublicCity SupportedCities.Budapest, PublicKdmidId id, PublicKdmidCd cd, PublicKdmidEms ems)
                    | Sarajevo ->
                        (PublicCity SupportedCities.Sarajevo, PublicKdmidId id, PublicKdmidCd cd, PublicKdmidEms ems)



        let createCredentials city id cd ems =

            let city' =
                match city with
                | PublicCity SupportedCities.Belgrade -> Ok Belgrade
                | PublicCity SupportedCities.Budapest -> Ok Budapest
                | PublicCity SupportedCities.Sarajevo -> Ok Sarajevo
                | _ -> Error "Invalid KDMID.City is not recognized."

            let id' =
                match id with
                | PublicKdmidId id when id > 1000 -> Ok <| KdmidId id
                | _ -> Error "Invalid KDMID.ID credential."

            let cd' =
                match cd with
                | PublicKdmidCd cd ->
                    match cd with
                    | Infrastructure.DSL.AP.IsLettersOrNumbers cd -> Ok <| KdmidCd cd
                    | _ -> Error "Invalid KDMID.CD credential."

            let ems' =
                match ems with
                | PublicKdmidEms ems ->
                    match ems with
                    | None -> Ok <| KdmidEms None
                    | Some ems ->
                        match ems with
                        | Infrastructure.DSL.AP.IsString ems -> Ok <| KdmidEms(Some ems)
                        | Infrastructure.DSL.AP.IsLettersOrNumbers ems -> Ok <| KdmidEms(Some ems)
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

        type Appointment =
            { Date: DateOnly
              Time: TimeOnly
              Description: string }

        type CredentialAppointments =
            { Credentials: Credentials
              Appointments: Appointment Set }

        type Error =
            | InvalidCredentials of string
            | InvalidResponse of string
            | InvalidRequest of string

    module User =
        open Kdmid

        type Type =
            | Admin
            | Regular

        type Id = UserId of string
        type User = { Id: Id; Name: string; Type: Type }

        type KdmidOrder = { User: User; Order: Credentials Set }

        type KdmidOrderResult =
            { User: User
              Result: CredentialAppointments }

module Persistence =
    module Kdmid =
        type Credentials =
            { City: string
              Id: int
              Cd: string
              Ems: string }

        type Appointment =
            { Date: DateTime
              Time: DateTime
              Description: string }

        type CredentialAppointments =
            { Credentials: Credentials
              Appointments: Appointment seq }

    module User =
        open Kdmid

        type User =
            { Id: string
              Name: string
              Type: string }

        type KdmidOrder =
            { User: User
              Credentials: Credentials seq }

        type KdmidOrderResult =
            { User: User
              Result: CredentialAppointments seq }
