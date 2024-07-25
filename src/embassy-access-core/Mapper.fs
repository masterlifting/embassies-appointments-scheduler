module EmbassyAccess.Mapper.Core

open System
open Infrastructure
open EmbassyAccess.Domain.Core

module Internal =
    let toCity (city: External.City) : Result<Internal.City, Error'> =
        match city.Name with
        | "Belgrade" -> Ok <| Internal.Belgrade
        | "Berlin" -> Ok <| Internal.Berlin
        | "Sarajevo" -> Ok <| Internal.Sarajevo
        | "Budapest" -> Ok <| Internal.Budapest
        | "Podgorica" -> Ok <| Internal.Podgorica
        | "Tirana" -> Ok <| Internal.Tirana
        | "Paris" -> Ok <| Internal.Paris
        | "Rome" -> Ok <| Internal.Rome
        | "Ljubljana" -> Ok <| Internal.Ljubljana
        | "Hague" -> Ok <| Internal.Hague
        | "Helsinki" -> Ok <| Internal.Helsinki
        | "Bern" -> Ok <| Internal.Bern
        | "Dublin" -> Ok <| Internal.Dublin
        | _ -> Error <| NotSupported $"City {city.Name}."

    let toCountry (country: External.Country) : Result<Internal.Country, Error'> =
        toCity country.City
        |> Result.bind (fun city ->
            match country.Name with
            | "Serbia" -> Ok <| Internal.Serbia city
            | "Bosnia" -> Ok <| Internal.Bosnia city
            | "Hungary" -> Ok <| Internal.Hungary city
            | "Montenegro" -> Ok <| Internal.Montenegro city
            | "Albania" -> Ok <| Internal.Albania city
            | "Germany" -> Ok <| Internal.Germany city
            | "France" -> Ok <| Internal.France city
            | "Switzerland" -> Ok <| Internal.Switzerland city
            | "Netherlands" -> Ok <| Internal.Netherlands city
            | "Ireland" -> Ok <| Internal.Ireland city
            | "Finland" -> Ok <| Internal.Finland city
            | "Slovenia" -> Ok <| Internal.Slovenia city
            | _ -> Error <| NotSupported $"Country {country.Name}.")

    let toEmbassy (embassy: External.Embassy) : Result<Internal.Embassy, Error'> =
        toCountry embassy.Country
        |> Result.bind (fun country ->
            match embassy.Name with
            | "Russian" -> Ok <| Internal.Russian country
            | _ -> Error <| NotSupported $"Embassy {embassy.Name}.")

    let toRequest (request: External.Request) : Result<Internal.Request, Error'> =
        toEmbassy request.Embassy
        |> Result.map (fun embassy ->
            { Id = Internal.RequestId request.Id
              Value = request.Value
              Attempt = request.Attempt
              Embassy = embassy
              Modified = request.Modified })

    let toAppointment (appointment: External.Appointment) : Internal.Appointment =
        { Id = Internal.AppointmentId appointment.Id
          Value = appointment.Value
          Date = DateOnly.FromDateTime(appointment.DateTime)
          Time = TimeOnly.FromDateTime(appointment.DateTime)
          Description =
            match appointment.Description with
            | "" -> None
            | x -> Some x }

    let toAppointmentsResponse
        (response: External.AppointmentsResponse)
        : Result<Internal.AppointmentsResponse, Error'> =
        toRequest response.Request
        |> Result.map (fun request ->
            { Id = Internal.ResponseId response.Id
              Request = request
              Appointments = response.Appointments |> Array.map toAppointment |> set
              Modified = response.Modified })

    let toConfirmationResponse
        (response: External.ConfirmationResponse)
        : Result<Internal.ConfirmationResponse, Error'> =
        toRequest response.Request
        |> Result.map (fun request ->
            { Id = Internal.ResponseId response.Id
              Request = request
              Description = response.Description
              Modified = response.Modified })

module External =

    let toCity (city: Internal.City) : External.City =
        let result = External.City()

        result.Name <-
            match city with
            | Internal.Belgrade -> "Belgrade"
            | Internal.Berlin -> "Berlin"
            | Internal.Sarajevo -> "Sarajevo"
            | Internal.Budapest -> "Budapest"
            | Internal.Podgorica -> "Podgorica"
            | Internal.Tirana -> "Tirana"
            | Internal.Paris -> "Paris"
            | Internal.Rome -> "Rome"
            | Internal.Ljubljana -> "Ljubljana"
            | Internal.Hague -> "Hague"
            | Internal.Helsinki -> "Helsinki"
            | Internal.Bern -> "Bern"
            | Internal.Dublin -> "Dublin"

        result

    let toCountry (country: Internal.Country) : External.Country =
        let result = External.Country()

        let countryName, city =
            match country with
            | Internal.Serbia city -> "Serbia", city
            | Internal.Germany city -> "Germany", city
            | Internal.Bosnia city -> "Bosnia", city
            | Internal.Hungary city -> "Hungary", city
            | Internal.Montenegro city -> "Montenegro", city
            | Internal.Albania city -> "Albania", city
            | Internal.Finland city -> "Finland", city
            | Internal.France city -> "France", city
            | Internal.Slovenia city -> "Slovenia", city
            | Internal.Switzerland city -> "Switzerland", city
            | Internal.Netherlands city -> "Netherlands", city
            | Internal.Ireland city -> "Ireland", city


        result.Name <- countryName
        result.City <- toCity city

        result

    let toEmbassy (embassy: Internal.Embassy) : External.Embassy =
        let result = External.Embassy()

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

    let toRequest (request: Internal.Request) : External.Request =
        let result = External.Request()

        result.Id <- request.Id.Value
        result.Embassy <- toEmbassy request.Embassy
        result.Value <- request.Value
        result.Attempt <- request.Attempt
        result.Modified <- request.Modified

        result

    let toAppointment (appointment: Internal.Appointment) : External.Appointment =
        let result = External.Appointment()

        result.Id <- appointment.Id.Value
        result.DateTime <- appointment.Date.ToDateTime(appointment.Time)
        result.Description <- appointment.Description |> Option.defaultValue ""

        result

    let toAppointmentsResponse (response: Internal.AppointmentsResponse) : External.AppointmentsResponse =
        let result = External.AppointmentsResponse()

        result.Id <- response.Id.Value
        result.RequestId <- response.Request.Id.Value
        result.Request <- toRequest response.Request
        result.Appointments <- response.Appointments |> Seq.map toAppointment |> Seq.toArray
        result.Modified <- response.Modified

        result

    let toConfirmationResponse (response: Internal.ConfirmationResponse) : External.ConfirmationResponse =
        let result = External.ConfirmationResponse()

        result.Id <- response.Id.Value
        result.RequestId <- response.Request.Id.Value
        result.Request <- toRequest response.Request
        result.Description <- response.Description
        result.Modified <- response.Modified

        result
