module Eas.Domain

open System

module Internal =

    module Core =

        type User = { Name: string }
        type CityModel = { Name: string }
        type CountryModel = { Name: string; City: CityModel }
        type EmbassyModel = { Name: string; Country: CountryModel }

        type City =
            | Belgrade
            | Budapest
            | Sarajevo
            | Podgorica
            | Tirana
            | Paris
            | Rome

            member this.Model =
                match this with
                | Belgrade -> { Name = nameof Belgrade }
                | Budapest -> { Name = nameof Budapest }
                | Sarajevo -> { Name = nameof Sarajevo }
                | Podgorica -> { Name = nameof Podgorica }
                | Tirana -> { Name = nameof Tirana }
                | Paris -> { Name = nameof Paris }
                | Rome -> { Name = nameof Rome }

        type Country =
            | Serbia of City
            | Bosnia of City
            | Montenegro of City
            | Albania of City
            | Hungary of City

            member this.Model =
                match this with
                | Serbia city ->
                    { Name = nameof Serbia
                      City = city.Model }
                | Bosnia city ->
                    { Name = nameof Bosnia
                      City = city.Model }
                | Montenegro city ->
                    { Name = nameof Montenegro
                      City = city.Model }
                | Albania city ->
                    { Name = nameof Albania
                      City = city.Model }
                | Hungary city ->
                    { Name = nameof Hungary
                      City = city.Model }

        type Embassy =
            | Russian of Country
            | Spanish of Country
            | Italian of Country
            | French of Country
            | German of Country
            | British of Country

            member this.Model =
                match this with
                | Russian country ->
                    { Name = nameof Russian
                      Country = country.Model }
                | Spanish country ->
                    { Name = nameof Spanish
                      Country = country.Model }
                | Italian country ->
                    { Name = nameof Italian
                      Country = country.Model }
                | French country ->
                    { Name = nameof French
                      Country = country.Model }
                | German country ->
                    { Name = nameof German
                      Country = country.Model }
                | British country ->
                    { Name = nameof British
                      Country = country.Model }

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
                    | Podgorica -> ("podgorica", id, cd, ems)
                    | Tirana -> ("tirana", id, cd, ems)
                    | Paris -> ("paris", id, cd, ems)
                    | Rome -> ("rome", id, cd, ems)

        let createCredentials url =
            url
            |> toUri
            |> Result.bind (fun uri ->
                match uri.Host.Split('.') with
                | hostParts when hostParts.Length < 3 -> Error <| Parsing $"City is not recognized {url}."
                | hostParts ->
                    uri
                    |> toQueryParams
                    |> Result.bind (fun paramsMap ->
                        let city =
                            match hostParts[0] with
                            | "belgrad" -> Ok Belgrade
                            | "budapest" -> Ok Budapest
                            | "sarajevo" -> Ok Sarajevo
                            | _ -> Error <| $"City {hostParts[0]} is not supported"

                        let id =
                            paramsMap
                            |> Map.tryFind "id"
                            |> Option.map (fun id ->
                                match id with
                                | IsInt id when id > 1000 -> Ok <| Id id
                                | _ -> Error $"Invalid id parameter ")
                            |> Option.defaultValue (Error $"Id parameter is missing")

                        let cd =
                            paramsMap
                            |> Map.tryFind "cd"
                            |> Option.map (fun cd ->
                                match cd with
                                | IsLettersOrNumbers cd -> Ok <| Cd cd
                                | _ -> Error $"Invalid cd parameter")
                            |> Option.defaultValue (Error $"Cd parameter is missing")

                        let ems =
                            match paramsMap.TryGetValue "ems" with
                            | false, _ -> Ok <| Ems None
                            | true, value ->
                                match value with
                                | IsLettersOrNumbers ems -> Ok <| Ems(Some ems)
                                | _ -> Error $"Invalid ems parameter in {url}."

                        match city, id, cd, ems with
                        | Ok city, Ok id, Ok cd, Ok ems ->
                            Ok
                                { City = city
                                  Id = id
                                  Cd = cd
                                  Ems = ems }
                        | _ ->
                            let errors =
                                let error =
                                    function
                                    | Error error -> error
                                    | _ -> String.Empty

                                [ error city; error id; error cd; error ems ]
                                |> Seq.filter (not << String.IsNullOrEmpty)
                                |> String.concat "."

                            Error <| Parsing $"Invalid parameters in {url}.{errors}."))

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
