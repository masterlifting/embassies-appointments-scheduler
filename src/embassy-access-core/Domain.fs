module EmbassyAccess.Domain.Core

open System

module Internal =

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

    type AppointmentId =
        | AppointmentId of Guid

        member this.Value =
            match this with
            | AppointmentId id -> id

    type City =
        | Belgrade
        | Berlin
        | Budapest
        | Sarajevo
        | Podgorica
        | Tirana
        | Paris
        | Rome
        | Dublin
        | Bern
        | Helsinki
        | Hague
        | Ljubljana

    type Country =
        | Serbia of City
        | Germany of City
        | Bosnia of City
        | Montenegro of City
        | Albania of City
        | Hungary of City
        | Ireland of City
        | Switzerland of City
        | Finland of City
        | France of City
        | Netherlands of City
        | Slovenia of City

    type Embassy =
        | Russian of Country
        | Spanish of Country
        | Italian of Country
        | French of Country
        | German of Country
        | British of Country

    type Appointment =
        { Id: AppointmentId
          Value: string
          Date: DateOnly
          Time: TimeOnly
          Description: string option }

    type ConfirmationOption =
        | FirstAvailable
        | Range of DateTime * DateTime
        | Appointment of Appointment

    type Request =
        { Id: RequestId
          Value: string
          Attempt: int
          Embassy: Embassy
          Modified: DateTime }

    type AppointmentsResponse =
        { Id: ResponseId
          Request: Request
          Appointments: Set<Appointment>
          Modified: DateTime }

    type ConfirmationResponse =
        { Id: ResponseId
          Request: Request
          Description: string
          Modified: DateTime }

    module Russian =
        open Infrastructure
        open Web.Domain
        open Web.Client

        module ErrorCodes =

            [<Literal>]
            let PageHasError = "PageHasError"

            [<Literal>]
            let NotConfirmed = "NotConfirmed"

        type StorageUdateRequest = Request -> Async<Result<unit, Error'>>
        type HttpGetStringRequest = Http.Request -> Http.Client -> Async<Result<Http.Response<string>, Error'>>
        type HttpGetBytesRequest = Http.Request -> Http.Client -> Async<Result<Http.Response<byte array>, Error'>>
        type HttpPostStringRequest = Http.Request -> Http.RequestContent -> Http.Client -> Async<Result<string, Error'>>
        type SolveCaptchaImage = byte array -> Async<Result<int, Error'>>

        type GetAppointmentsDeps =
            { updateRequest: StorageUdateRequest 
              getInitialPage: HttpGetStringRequest
              getCaptcha: HttpGetBytesRequest
              solveCaptcha: SolveCaptchaImage
              postValidationPage: HttpPostStringRequest
              postAppointmentsPage: HttpPostStringRequest }

        type BookAppointmentDeps =
            { GetAppointmentsDeps: GetAppointmentsDeps
              postConfirmationPage: HttpPostStringRequest }

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
                    | Berlin -> ("berlin", id, cd, ems)
                    | Dublin -> ("dublin", id, cd, ems)
                    | Bern -> ("bern", id, cd, ems)
                    | Helsinki -> ("helsinki", id, cd, ems)
                    | Hague -> ("hague", id, cd, ems)
                    | Ljubljana -> ("ljubljana", id, cd, ems)

        let createCredentials url =
            url
            |> Http.Route.toUri
            |> Result.bind (fun uri ->
                match uri.Host.Split '.' with
                | hostParts when hostParts.Length < 3 -> Error <| NotSupported $"Kdmid. City in {url}."
                | hostParts ->
                    uri
                    |> Http.Route.toQueryParams
                    |> Result.bind (fun paramsMap ->
                        let city =
                            match hostParts[0] with
                            | "belgrad" -> Ok Belgrade
                            | "budapest" -> Ok Budapest
                            | "sarajevo" -> Ok Sarajevo
                            | "berlin" -> Ok Berlin
                            | "podgorica" -> Ok Podgorica
                            | "tirana" -> Ok Tirana
                            | "paris" -> Ok Paris
                            | "rome" -> Ok Rome
                            | "dublin" -> Ok Dublin
                            | "bern" -> Ok Bern
                            | "helsinki" -> Ok Helsinki
                            | "hague" -> Ok Hague
                            | "ljubljana" -> Ok Ljubljana
                            | _ -> Error $"City {hostParts[0]} is not supported"

                        let id =
                            paramsMap
                            |> Map.tryFind "id"
                            |> Option.map (function
                                | AP.IsInt id when id > 1000 -> Ok <| Id id
                                | _ -> Error $"Invalid id parameter ")
                            |> Option.defaultValue (Error $"Id parameter is missing")

                        let cd =
                            paramsMap
                            |> Map.tryFind "cd"
                            |> Option.map (function
                                | AP.IsLettersOrNumbers cd -> Ok <| Cd cd
                                | _ -> Error $"Invalid cd parameter")
                            |> Option.defaultValue (Error $"Cd parameter is missing")

                        let ems =
                            paramsMap
                            |> Map.tryFind "ems"
                            |> Option.map (function
                                | AP.IsLettersOrNumbers ems -> Ok <| Ems(Some ems)
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

                            Error <| NotSupported $"Parameters in {url}. {errors}."))

module External =

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
        member val Value: string = String.Empty with get, set
        member val Attempt: int = 0 with get, set
        member val UserId: int = 0 with get, set
        member val EmbassyId: int = 0 with get, set
        member val Embassy: Embassy = Embassy() with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    type AppointmentsResponse() =
        member val Id: Guid = Guid.Empty with get, set
        member val RequestId: Guid = Guid.Empty with get, set
        member val Request: Request = Request() with get, set
        member val Appointments: Appointment array = [||] with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set

    and Appointment() =
        member val Id: Guid = Guid.Empty with get, set
        member val Value: string = String.Empty with get, set
        member val DateTime: DateTime = DateTime.UtcNow with get, set
        member val Description: string = String.Empty with get, set

    type ConfirmationResponse() =
        member val Id: Guid = Guid.Empty with get, set
        member val RequestId: Guid = Guid.Empty with get, set
        member val Request: Request = Request() with get, set
        member val Description: string = String.Empty with get, set
        member val Modified: DateTime = DateTime.UtcNow with get, set
