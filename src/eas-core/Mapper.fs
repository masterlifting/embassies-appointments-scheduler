module Eas.Mapper

open Infrastructure.Domain.Errors
open Eas.Domain

module Internal =
    open Eas.Domain.Internal.Embassies.Russian
    open System

    let toUser (user: External.User) : Internal.User =
        { Id = Internal.UserId user.Id
          Name = user.Name }

    let toCity (city: External.City) : Result<Internal.City, InfrastructureError> =
        match city.Name with
        | "Belgrade" -> Ok <| Internal.Belgrade
        | "Sarajevo" -> Ok <| Internal.Sarajevo
        | "Budapest" -> Ok <| Internal.Budapest
        | "Podgorica" -> Ok <| Internal.Podgorica
        | "Tirana" -> Ok <| Internal.Tirana
        | "Paris" -> Ok <| Internal.Paris
        | "Rome" -> Ok <| Internal.Rome
        | _ -> Error <| (Mapping $"City {city.Name} not supported.")

    let toCountry (country: External.Country) : Result<Internal.Country, InfrastructureError> =
        toCity country.City
        |> Result.bind (fun city ->
            match country.Name with
            | "Serbia" -> Ok <| Internal.Serbia city
            | "Bosnia" -> Ok <| Internal.Bosnia city
            | "Hungary" -> Ok <| Internal.Hungary city
            | "Montenegro" -> Ok <| Internal.Montenegro city
            | "Albania" -> Ok <| Internal.Albania city
            | _ -> Error <| (Mapping $"Country {country.Name} not supported."))

    let toEmbassy (embassy: External.Embassy) : Result<Internal.Embassy, InfrastructureError> =
        toCountry embassy.Country
        |> Result.bind (fun country ->
            match embassy.Name with
            | "Russian" -> Ok <| Internal.Russian country
            | _ -> Error <| (Mapping $"Embassy {embassy.Name} not supported."))


    let toRequest (request: External.Request) : Result<Internal.Request, InfrastructureError> =
        toEmbassy request.Embassy
        |> Result.bind (fun embassy ->
            Ok
                { Id = Internal.RequestId request.Id
                  Embassy = embassy
                  Data = request.Data |> Array.map (fun x -> x.Key, x.Value) |> Map.ofArray
                  Modified = request.Modified })

    let toAppointment (appointment: External.Appointment) : Internal.Appointment =
        { Id = Internal.AppointementId appointment.Id
          Date = DateOnly.FromDateTime(appointment.DateTime)
          Time = TimeOnly.FromDateTime(appointment.DateTime)
          Description = appointment.Description }

    let toResponse (response: External.Response) : Result<Internal.Response, InfrastructureError> =
        toEmbassy response.Request.Embassy
        |> Result.bind (fun embassy ->
            Ok
                { Id = Internal.ResponseId response.Id
                  Embassy = embassy
                  Appointments = response.Appointments |> Array.map toAppointment |> set
                  Data = response.Data |> Array.map (fun x -> x.Key, x.Value) |> Map.ofArray
                  Modified = response.Modified })

module External =

    let toUser (user: Internal.User) : External.User =
        let result = new External.User()

        result.Id <-
            user.Id
            |> function
                | Internal.UserId id -> id

        result.Name <- user.Name
        result

    let toCity (city: Internal.City) : External.City =
        let result = new External.City()

        result.Name <-
            match city with
            | Internal.Belgrade -> "Belgrade"
            | Internal.Sarajevo -> "Sarajevo"
            | Internal.Budapest -> "Budapest"
            | Internal.Podgorica -> "Podgorica"
            | Internal.Tirana -> "Tirana"
            | Internal.Paris -> "Paris"
            | Internal.Rome -> "Rome"

        result

    let toCountry (country: Internal.Country) : External.Country =
        let result = new External.Country()

        let countryName, city =
            match country with
            | Internal.Serbia city -> "Serbia", city
            | Internal.Bosnia city -> "Bosnia", city
            | Internal.Hungary city -> "Hungary", city
            | Internal.Montenegro city -> "Montenegro", city
            | Internal.Albania city -> "Albania", city

        result.Name <- countryName
        result.City <- toCity city
        result

    let toEmbassy (embassy: Internal.Embassy) : External.Embassy =
        let result = new External.Embassy()

        let embassyName, country =
            match embassy with
            | Internal.Russian country -> "Russian", country
            | Internal.French country -> "French", country
            | Internal.Italian country -> "Italian", country
            | Internal.Spanish country -> "Spanish", country
            | Internal.German country -> "German", country
            | Internal.British country -> "British", country

        result.Name <- embassyName
        result.Country <- toCountry country
        result

    let toRequest (user: Internal.User) (request: Internal.Request) : External.Request =
        let result = new External.Request()

        result.Id <-
            request.Id
            |> function
                | Internal.RequestId id -> id

        result.UserId <-
            user.Id
            |> function
                | Internal.UserId id -> id

        result.User <- toUser user
        result.Embassy <- toEmbassy request.Embassy

        result.Data <-
            request.Data
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                let data = new External.RequestData()
                data.Key <- key
                data.Value <- value
                data)
            |> Seq.toArray

        result.Modified <- request.Modified

        result

    let toAppointment (appointment: Internal.Appointment) : External.Appointment =
        let result = new External.Appointment()

        result.Id <-
            appointment.Id
            |> function
                | Internal.AppointementId id -> id

        result.DateTime <- appointment.Date.ToDateTime(appointment.Time)
        result.Description <- appointment.Description
        result

    let toResponse (user: Internal.User) (request: Internal.Request) (response: Internal.Response) : External.Response =
        let result = new External.Response()

        result.Id <-
            response.Id
            |> function
                | Internal.ResponseId id -> id

        result.RequestId <-
            request.Id
            |> function
                | Internal.RequestId id -> id

        result.Request <- toRequest user request
        result.Appointments <- response.Appointments |> Seq.map toAppointment |> Seq.toArray

        result.Data <-
            response.Data
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                let data = new External.ResponseData()
                data.Key <- key
                data.Value <- value
                data)
            |> Seq.toArray

        result.Modified <- response.Modified

        result
