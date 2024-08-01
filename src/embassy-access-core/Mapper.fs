module EmbassyAccess.Mapper

open System
open Infrastructure
open EmbassyAccess.Domain

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
    | "Ljubljana" -> Ok <| Ljubljana
    | "Hague" -> Ok <| Hague
    | "Helsinki" -> Ok <| Helsinki
    | "Bern" -> Ok <| Bern
    | "Dublin" -> Ok <| Dublin
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
        | "France" -> Ok <| France city
        | "Switzerland" -> Ok <| Switzerland city
        | "Netherlands" -> Ok <| Netherlands city
        | "Ireland" -> Ok <| Ireland city
        | "Finland" -> Ok <| Finland city
        | "Slovenia" -> Ok <| Slovenia city
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
          Value = request.Value
          Attempt = request.Attempt
          Embassy = embassy
          Modified = request.Modified })

let toAppointment (appointment: External.Appointment) : Appointment =
    { Id = AppointmentId appointment.Id
      Value = appointment.Value
      Date = DateOnly.FromDateTime(appointment.DateTime)
      Time = TimeOnly.FromDateTime(appointment.DateTime)
      Description =
        match appointment.Description with
        | "" -> None
        | x -> Some x }

let toAppointmentsResponse (response: External.AppointmentsResponse) : Result<AppointmentsResponse, Error'> =
    toRequest response.Request
    |> Result.map (fun request ->
        { Id = ResponseId response.Id
          Request = request
          Appointments = response.Appointments |> Array.map toAppointment |> set
          Modified = response.Modified })

let toConfirmationResponse (response: External.ConfirmationResponse) : Result<ConfirmationResponse, Error'> =
    toRequest response.Request
    |> Result.map (fun request ->
        { Id = ResponseId response.Id
          Request = request
          Description = response.Description
          Modified = response.Modified })

module External =

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
            | Ljubljana -> "Ljubljana"
            | Hague -> "Hague"
            | Helsinki -> "Helsinki"
            | Bern -> "Bern"
            | Dublin -> "Dublin"

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
            | Finland city -> "Finland", city
            | France city -> "France", city
            | Slovenia city -> "Slovenia", city
            | Switzerland city -> "Switzerland", city
            | Netherlands city -> "Netherlands", city
            | Ireland city -> "Ireland", city


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
        result.Embassy <- toEmbassy request.Embassy
        result.Value <- request.Value
        result.Attempt <- request.Attempt
        result.Modified <- request.Modified

        result

    let toAppointment (appointment: Appointment) : External.Appointment =
        let result = External.Appointment()

        result.Id <- appointment.Id.Value
        result.DateTime <- appointment.Date.ToDateTime(appointment.Time)
        result.Description <- appointment.Description |> Option.defaultValue ""

        result

    let toAppointmentsResponse (response: AppointmentsResponse) : External.AppointmentsResponse =
        let result = External.AppointmentsResponse()

        result.Id <- response.Id.Value
        result.RequestId <- response.Request.Id.Value
        result.Request <- toRequest response.Request
        result.Appointments <- response.Appointments |> Seq.map toAppointment |> Seq.toArray
        result.Modified <- response.Modified

        result

    let toConfirmationResponse (response: ConfirmationResponse) : External.ConfirmationResponse =
        let result = External.ConfirmationResponse()

        result.Id <- response.Id.Value
        result.RequestId <- response.Request.Id.Value
        result.Request <- toRequest response.Request
        result.Description <- response.Description
        result.Modified <- response.Modified

        result
