module KdmidScheduler.Domain

open System
open Infrastructure.Domain.Errors

module Core =
    module Embassies =
        type Request = Request of string

        type City =
            | Belgrade
            | Budapest
            | Sarajevo
            | Paris
            | Rome

        type Country =
            | Serbia of City
            | Hungary of City
            | Bosnia of City

        type Embassy =
            | Russian of Country
            | Serbian of Country
            | Hungarian of Country
            | French of Country
            | Italian of Country
            | Albanian of Country

        type Appointment =
            { Date: DateOnly
              Time: TimeOnly
              Description: string }

        type AppointmentResult =
            { Embassy: Embassy
              Appointments: Appointment Set }

        module Russian =
            type Id = private Id of int
            type Cd = private Cd of string
            type Ems = private Ems of string option

            type Credentials =
                { City: City
                  Id: Id
                  Cd: Cd
                  Ems: Ems }

                member this.Deconstructed =
                    match this with
                    | { City = city
                        Id = Id id
                        Cd = Cd cd
                        Ems = Ems ems } ->
                        match city with
                        | Belgrade -> ("belgrade", id, cd, ems)
                        | Budapest -> ("budapest", id, cd, ems)
                        | Sarajevo -> ("sarajevo", id, cd, ems)
                        | Paris -> ("paris", id, cd, ems)
                        | Rome -> ("rome", id, cd, ems)

            let createCredentials url =
                match url with
                | Request url ->
                    Web.Core.Http.getUri url
                    |> Result.bind (fun uri ->
                        match uri.Host.Split('.') with
                        | hostParts when hostParts.Length < 3 -> Error "City is not recognized in URL."
                        | hostParts ->
                            let city =
                                match hostParts[0] with
                                | "belgrad" -> Ok Belgrade
                                | "budapest" -> Ok Budapest
                                | "sarajevo" -> Ok Sarajevo
                                | _ -> Error "City is not supported."

                            match Web.Core.Http.getQueryParameters uri with
                            | Ok paramsMap when paramsMap.Keys |> Seq.forall (fun key -> key = "id" || key = "cd") ->
                                let id =
                                    match paramsMap["id"] with
                                    | Infrastructure.DSL.AP.IsInt id when id > 1000 -> Ok <| Id id
                                    | _ -> Error "Invalid id parameter in URL."

                                let cd =
                                    match paramsMap["cd"] with
                                    | Infrastructure.DSL.AP.IsLettersOrNumbers cd -> Ok <| Cd cd
                                    | _ -> Error "Invalid cd parameter in URL."

                                let ems =
                                    match paramsMap.TryGetValue "ems" with
                                    | false, _ -> Ok <| Ems None
                                    | true, value ->
                                        match value with
                                        | Infrastructure.DSL.AP.IsLettersOrNumbers ems -> Ok <| Ems(Some ems)
                                        | _ -> Error "Invalid ems parameter in URL."

                                city
                                |> Result.bind (fun city ->
                                    id
                                    |> Result.bind (fun id ->
                                        cd
                                        |> Result.bind (fun cd ->
                                            ems
                                            |> Result.map (fun ems ->
                                                { City = city
                                                  Id = id
                                                  Cd = cd
                                                  Ems = ems }))))
                            | _ -> Error "Invalid query parameters in URL.")

            type Error =
                | InfrastructureError of InfrastructureError
                | InvalidCredentials of string

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
