module EmbassyAccess.Embassies.Russian.Domain

open Infrastructure
open EmbassyAccess
open EmbassyAccess.Domain.Internal

module ErrorCodes =

    [<Literal>]
    let PageHasError = "PageHasError"

    [<Literal>]
    let NotConfirmed = "NotConfirmed"

type StorageUpdateRequest = Domain.Internal.Request -> Async<Result<unit, Error'>>

type HttpGetStringRequest =
    Web.Http.Domain.Request -> Web.Http.Domain.Client -> Async<Result<Web.Http.Domain.Response<string>, Error'>>

type HttpGetBytesRequest =
    Web.Http.Domain.Request -> Web.Http.Domain.Client -> Async<Result<Web.Http.Domain.Response<byte array>, Error'>>

type HttpPostStringRequest =
    Web.Http.Domain.Request -> Web.Http.Domain.RequestContent -> Web.Http.Domain.Client -> Async<Result<string, Error'>>

type SolveCaptchaImage = byte array -> Async<Result<int, Error'>>

type GetAppointmentsDeps =
    { updateRequest: StorageUpdateRequest
      getInitialPage: HttpGetStringRequest
      getCaptcha: HttpGetBytesRequest
      solveCaptcha: SolveCaptchaImage
      postValidationPage: HttpPostStringRequest
      postAppointmentsPage: HttpPostStringRequest }

type BookAppointmentDeps =
    { GetAppointmentsDeps: GetAppointmentsDeps
      postConfirmationPage: HttpPostStringRequest }

type TryGetAppointments =
    { getAppointments: Domain.Internal.Request -> Async<Result<Domain.Internal.AppointmentsResponse option, Error'>> }

type Id = private Id of int
type Cd = private Cd of string
type Ems = private Ems of string option

type Credentials =
    { City: Domain.Internal.City
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
    |> Web.Http.Client.Route.toUri
    |> Result.bind (fun uri ->
        match uri.Host.Split '.' with
        | hostParts when hostParts.Length < 3 -> Error <| NotSupported $"Kdmid. City in {url}."
        | hostParts ->
            uri
            |> Web.Http.Client.Route.toQueryParams
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
