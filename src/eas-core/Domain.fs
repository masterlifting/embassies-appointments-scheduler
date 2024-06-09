module Eas.Domain

open System

module Internal =

    module Core =

        type User = { Id: int; Name: string }

        type City =
            | Belgrade
            | Budapest
            | Sarajevo
            | Podgorica
            | Tirana
            | Paris
            | Rome

        type Country =
            | Serbia of City
            | Bosnia of City
            | Montenegro of City
            | Albania of City
            | Hungary of City

        type Embassy =
            | Russian of Country
            | Spanish of Country
            | Italian of Country
            | French of Country
            | German of Country
            | British of Country

        type Appointment =
            { Date: DateOnly
              Time: TimeOnly
              Description: string }

        type Request = { Embassy: Embassy; Data: string }

        type Response =
            { Embassy: Embassy
              Appointments: Set<Appointment>
              Data: Map<string, string> }

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
            url
            |> toUri
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
            |> Result.mapError Parsing

module External =

    type User() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set

    type City() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set

    type Country() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set
        member val CityId: int = 0 with get, set
        member val City: City = City() with get, set

    type Embassy() =
        member val Id: int = 0 with get, set
        member val Name: string = String.Empty with get, set
        member val CountryId: int = 0 with get, set
        member val Country: Country = Country() with get, set

    type Request() =
        member val Id: int = 0 with get, set
        member val Data: string = String.Empty with get, set
        member val EmbassyId: int = 0 with get, set
        member val Embassy: Embassy = Embassy() with get, set
        member val UserId: int = 0 with get, set
        member val User: User = User() with get, set

    type Response() =
        member val Id: int = 0 with get, set
        member val Appointments: Appointment array = [||] with get, set
        member val Data: ResponseData array = [||] with get, set
        member val RequestId: int = 0 with get, set
        member val Request: Request = Request() with get, set
        member val IsConfirmed: bool = false with get, set

    and Appointment() =
        member val Id: int = 0 with get, set
        member val Date: DateTime = DateTime.MinValue with get, set
        member val Time: DateTime = DateTime.MinValue with get, set
        member val Description: string = String.Empty with get, set
        member val ResponseId: int = 0 with get, set
        member val Response: Response = Response() with get, set

    and ResponseData() =
        member val Id: int = 0 with get, set
        member val Key: string = String.Empty with get, set
        member val Value: string = String.Empty with get, set
        member val ResponseId: int = 0 with get, set
        member val Response: Response = Response() with get, set
