module Eas.Domain

open System

module Internal =

    module Core =

        type Embassy =
            | Russian
            | Spanish
            | Italian
            | French
            | German
            | British

        type Country =
            | Serbia
            | Bosnia
            | Montenegro
            | Albania
            | Hungary

        type City =
            | Belgrade
            | Budapest
            | Sarajevo
            | Podgorica
            | Tirana
            | Paris
            | Rome

        type Request = string

        type Appointment =
            { Date: DateOnly
              Time: TimeOnly
              Description: string }

    module Russian =
        open Core
        open Web.Core.Http.Mapper
        open Infrastructure.DSL.ActivePatterns
        open Infrastructure.Domain.Errors


        type Id = private Id of int
        type Cd = private Cd of string
        type Ems = private Ems of string option

        type Credentials =
            { City: City
              Id: Id
              Cd: Cd
              Ems: Ems }

            member this.Value =
                match this with
                | { City = city
                    Id = Id id
                    Cd = Cd cd
                    Ems = Ems ems } ->
                    match city with
                    | Belgrade -> ("belgrad", id, cd, ems)
                    | Budapest -> ("budapest", id, cd, ems)
                    | Sarajevo -> ("sarajevo", id, cd, ems)
                    | Paris -> ("paris", id, cd, ems)
                    | Rome -> ("rome", id, cd, ems)
                    | Podgorica -> ("podgorica", id, cd, ems)
                    | Tirana -> ("tirana", id, cd, ems)

        let createCredentials url =
            toUri url
            |> Result.bind (fun uri ->
                match uri.Host.Split('.') with
                | hostParts when hostParts.Length < 3 -> Error $"City is not recognized in url: {url}."
                | hostParts ->
                    let city =
                        match hostParts[0] with
                        | "belgrad" -> Ok Belgrade
                        | "budapest" -> Ok Budapest
                        | "sarajevo" -> Ok Sarajevo
                        | _ -> Error $"City {hostParts[0]} is not supported for url: {url}."

                    match toQueryParams uri with
                    | Ok paramsMap when paramsMap.Keys |> Seq.forall (fun key -> key = "id" || key = "cd") ->
                        let id =
                            match paramsMap["id"] with
                            | IsInt id when id > 1000 -> Ok <| Id id
                            | _ -> Error $"Invalid id parameter in url: {url}."

                        let cd =
                            match paramsMap["cd"] with
                            | IsLettersOrNumbers cd -> Ok <| Cd cd
                            | _ -> Error $"Invalid cd parameter in url: {url}."

                        let ems =
                            match paramsMap.TryGetValue "ems" with
                            | false, _ -> Ok <| Ems None
                            | true, value ->
                                match value with
                                | IsLettersOrNumbers ems -> Ok <| Ems(Some ems)
                                | _ -> Error $"Invalid ems parameter in url: {url}."

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
                    | _ -> Error $"Invalid query parameters in url: {url}.")
            |> Result.mapError (fun error -> Parsing error)

module External =
    type City = { Name: string }
    type Country = { Name: string }
    type Embassy = { Name: string }
