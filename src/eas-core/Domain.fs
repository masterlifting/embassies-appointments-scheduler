module Eas.Domain

open System

module Internal =

    type UserId =
        | UserId of int

        member this.Value =
            match this with
            | UserId id -> id

    type AppointementId =
        | AppointementId of Guid

        member this.Value =
            match this with
            | AppointementId id -> id

    type RequestId =
        | RequestId of Guid

        member this.Value =
            match this with
            | RequestId id -> id

    type ResponseId =
        | ResponseId of Guid

        member this.Value =
            match this with
            | ResponseId id -> id

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

    type User = { Id: UserId; Name: string }

    type Request =
        { Id: RequestId
          User: User
          Embassy: Embassy
          Data: Map<string, string>
          Modified: DateTime }

    type Appointment =
        { Id: AppointementId
          Date: DateOnly
          Time: TimeOnly
          Description: string }

    type Response =
        { Id: ResponseId
          Request: Request
          Appointments: Set<Appointment>
          Data: Map<string, string>
          Modified: DateTime }

    module Embassies =

        module Russian =
            open Web.Client.Http
            open Infrastructure.Dsl.ActivePatterns
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
                    | hostParts when hostParts.Length < 3 -> Error(Parsing $"City is not recognized {url}.")
                    | hostParts ->
                        uri
                        |> toQueryParams
                        |> Result.bind (fun paramsMap ->
                            let city =
                                match hostParts[0] with
                                | "belgrad" -> Ok Belgrade
                                | "budapest" -> Ok Budapest
                                | "sarajevo" -> Ok Sarajevo
                                | _ -> Error $"City {hostParts[0]} is not supported"

                            let id =
                                paramsMap
                                |> Map.tryFind "id"
                                |> Option.map (function
                                    | IsInt id when id > 1000 -> Ok <| Id id
                                    | _ -> Error $"Invalid id parameter ")
                                |> Option.defaultValue (Error $"Id parameter is missing")

                            let cd =
                                paramsMap
                                |> Map.tryFind "cd"
                                |> Option.map (function
                                    | IsLettersOrNumbers cd -> Ok <| Cd cd
                                    | _ -> Error $"Invalid cd parameter")
                                |> Option.defaultValue (Error $"Cd parameter is missing")

                            let ems =
                                paramsMap
                                |> Map.tryFind "ems"
                                |> Option.map (function
                                    | IsLettersOrNumbers ems -> Ok <| Ems(Some ems)
                                    | _ -> Error $"Invalid ems parameter")
                                |> Option.defaultValue (Ok <| Ems None)

                            match city, id, cd, ems with
                            | Ok city, Ok id, Ok cd, Ok ems ->
                                let credentials: Credentials =
                                    { City = city
                                      Id = id
                                      Cd = cd
                                      Ems = ems }

                                Ok credentials
                            | _ ->
                                let errors =
                                    let error =
                                        function
                                        | Error error -> Some error
                                        | _ -> None

                                    [ error city; error id; error cd; error ems ]
                                    |> List.choose Operators.id
                                    |> List.fold (fun acc error -> $"{acc},{error}") ""

                                Error(Parsing $"Invalid parameters in {url}.{errors}.")))

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
        member val Id: Guid = Guid.Empty with get, set
        member val UserId: int = 0 with get, set
        member val User: User = User() with get, set
        member val EmbassyId: int = 0 with get, set
        member val Embassy: Embassy = Embassy() with get, set
        member val Data: RequestData array = [||] with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    and RequestData() =
        member val Id: int = 0 with get, set
        member val Key: string = String.Empty with get, set
        member val Value: string = String.Empty with get, set

    type Response() =
        member val Id: Guid = Guid.Empty with get, set
        member val Confirmed: bool = false with get, set
        member val RequestId: Guid = Guid.Empty with get, set
        member val Request: Request = Request() with get, set
        member val Appointments: Appointment array = [||] with get, set
        member val Data: ResponseData array = [||] with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    and Appointment() =
        member val Id: Guid = Guid.Empty with get, set
        member val DateTime: DateTime = DateTime.UtcNow with get, set
        member val Description: string = String.Empty with get, set

    and ResponseData() =
        member val Id: int = 0 with get, set
        member val Key: string = String.Empty with get, set
        member val Value: string = String.Empty with get, set
