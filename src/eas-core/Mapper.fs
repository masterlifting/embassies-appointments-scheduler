module Eas.Mapper

open System
open Eas.Domain.Internal
open Infrastructure.Domain.Errors
open Eas.Domain

module Internal =

    let toUser (user: External.User) : User =
        { Id = UserId user.Id
          Name = user.Name }

    let toCity (city: External.City) : Result<City, Error'> =
        match city.Name with
        | "Belgrade" -> Ok <| Belgrade
        | "Berlin" -> Ok <| Berlin
        | "Sarajevo" -> Ok <| Sarajevo
        | "Budapest" -> Ok <| Budapest
        | "Podgorica" -> Ok <| Podgorica
        | "Tirana" -> Ok <| Tirana
        | "Paris" -> Ok <| Paris
        | "Rome" -> Ok <| Rome
        | _ -> Error <| NotSupported $"City {city.Name}."

    let toCountry (country: External.Country) : Result<Country, Error'> =
        toCity country.City
        |> Result.bind (fun city ->
            match country.Name with
            | "Serbia" -> Ok <| Serbia city
            | "Bosnia" -> Ok <| Bosnia city
            | "Hungary" -> Ok <| Hungary city
            | "Montenegro" -> Ok <| Montenegro city
            | "Albania" -> Ok <| Albania city
            | "Germany" -> Ok <| Germany city
            | _ -> Error <| NotSupported $"Country {country.Name}.")

    let toEmbassy (embassy: External.Embassy) : Result<Embassy, Error'> =
        toCountry embassy.Country
        |> Result.bind (fun country ->
            match embassy.Name with
            | "Russian" -> Ok <| Russian country
            | _ -> Error <| NotSupported $"Embassy {embassy.Name}.")

    let toRequest (request: External.Request) : Result<Request, Error'> =
        toEmbassy request.Embassy
        |> Result.map (fun embassy ->
            { Id = RequestId request.Id
              User = toUser request.User
              Embassy = embassy
              Data = request.Data |> Array.map (fun x -> x.Key, x.Value) |> Map.ofArray
              Modified = request.Modified })

    let toAppointment (appointment: External.Appointment) : Appointment =
        { Id = AppointmentId appointment.Id
          Date = DateOnly.FromDateTime(appointment.DateTime)
          Time = TimeOnly.FromDateTime(appointment.DateTime)
          Description = appointment.Description }

    let toResponse (response: External.Response) : Result<Response, Error'> =
        toRequest response.Request
        |> Result.map (fun request ->
            { Id = ResponseId response.Id
              Request = request
              Appointments = response.Appointments |> Array.map toAppointment |> set
              Data = response.Data |> Array.map (fun x -> x.Key, x.Value) |> Map.ofArray
              Modified = response.Modified })

module External =

    let toUser (user: User) : External.User =
        let result = External.User()

        result.Id <- user.Id.Value
        result.Name <- user.Name

        result

    let toCity (city: City) : External.City =
        let result = External.City()

        result.Name <-
            match city with
            | Belgrade -> "Belgrade"
            | Berlin -> "Berlin"
            | Sarajevo -> "Sarajevo"
            | Budapest -> "Budapest"
            | Podgorica -> "Podgorica"
            | Tirana -> "Tirana"
            | Paris -> "Paris"
            | Rome -> "Rome"

        result

    let toCountry (country: Country) : External.Country =
        let result = External.Country()

        let countryName, city =
            match country with
            | Serbia city -> "Serbia", city
            | Germany city -> "Germany", city
            | Bosnia city -> "Bosnia", city
            | Hungary city -> "Hungary", city
            | Montenegro city -> "Montenegro", city
            | Albania city -> "Albania", city

        result.Name <- countryName
        result.City <- toCity city

        result

    let toEmbassy (embassy: Embassy) : External.Embassy =
        let result = External.Embassy()

        let embassyName, country =
            match embassy with
            | Russian country -> "Russian", country
            | French country -> "French", country
            | Italian country -> "Italian", country
            | Spanish country -> "Spanish", country
            | German country -> "German", country
            | British country -> "British", country

        result.Name <- embassyName
        result.Country <- toCountry country

        result

    let toRequest (request: Request) : External.Request =
        let result = External.Request()

        result.Id <- request.Id.Value
        result.UserId <- request.User.Id.Value

        result.User <- toUser request.User
        result.Embassy <- toEmbassy request.Embassy

        result.Data <-
            request.Data
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                let data = External.RequestData()
                data.Key <- key
                data.Value <- value
                data)
            |> Seq.toArray

        result.Modified <- request.Modified

        result

    let toAppointment (appointment: Appointment) : External.Appointment =
        let result = External.Appointment()

        result.Id <- appointment.Id.Value
        result.DateTime <- appointment.Date.ToDateTime(appointment.Time)
        result.Description <- appointment.Description

        result

    let toResponse (response: Response) : External.Response =
        let result = External.Response()

        result.Id <- response.Id.Value
        result.RequestId <- response.Request.Id.Value
        result.Request <- toRequest response.Request
        result.Appointments <- response.Appointments |> Seq.map toAppointment |> Seq.toArray

        result.Data <-
            response.Data
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                let data = External.ResponseData()
                data.Key <- key
                data.Value <- value
                data)
            |> Seq.toArray

        result.Modified <- response.Modified

        result
