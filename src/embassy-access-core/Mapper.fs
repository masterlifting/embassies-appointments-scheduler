module internal EmbassyAccess.Mapper

open System
open Infrastructure
open EmbassyAccess.Domain

let toCity (city: External.City) : Result<City, Error'> =
    Reflection.getUnionCases<City> ()
    |> Result.bind (fun cities ->
        match cities |> Array.tryFind (fun city -> city.Name = city.Name) with
        | Some city -> Ok city
        | None -> Error <| NotSupported $"City {city.Name}.")

let toCountry (country: External.Country) : Result<Country, Error'> =
    toCity country.City
    |> Result.bind (fun city ->
        Reflection.getUnionCases<Country> ()
        |> Result.bind (fun countries ->
            match countries |> Array.tryFind (fun country -> country.City = city) with
            | Some country -> Ok country
            | None -> Error <| NotSupported $"Country {country.Name}."))

let toEmbassy (embassy: External.Embassy) : Result<Embassy, Error'> =
    toCountry embassy.Country
    |> Result.bind (fun country ->
        Reflection.getUnionCases<Embassy> ()
        |> Result.bind (fun embassies ->
            match embassies |> Array.tryFind (fun embassy -> embassy.Country = country) with
            | Some embassy -> Ok embassy
            | None -> Error <| NotSupported $"Embassy {embassy.Name}."))

let toConfirmation (confirmation: External.Confirmation option) : Confirmation option =
    confirmation |> Option.map (fun x -> { Description = x.Description })

let toAppointment (appointment: External.Appointment) : Appointment =
    { Value = appointment.Value
      Date = DateOnly.FromDateTime(appointment.DateTime)
      Time = TimeOnly.FromDateTime(appointment.DateTime)
      Confirmation = appointment.Confirmation |> toConfirmation
      Description =
        match appointment.Description with
        | AP.IsString x -> Some x
        | _ -> None }

let toRequest (request: External.Request) : Result<Request, Error'> =
    toEmbassy request.Embassy
    |> Result.bind (fun embassy ->
        Reflection.getUnionCases<RequestState> ()
        |> Result.bind (fun states ->
            match states |> Array.tryFind (fun state -> state.Name = request.State) with
            | Some state ->
                { Id = RequestId request.Id
                  Value = request.Value
                  Attempt = request.Attempt
                  State = state
                  Embassy = embassy
                  Appointments = request.Appointments |> Seq.map toAppointment |> Set.ofSeq
                  Description = 
                    match request.Description with
                    | AP.IsString x -> Some x
                    | _ -> None
                  Modified = request.Modified }
                |> Ok
            | None -> Error <| NotSupported $"Request state {request.State}."))

module External =

    let toCity (city: City) : External.City =
        let result = External.City()
        
        result.Name <- city.Name
        
        result

    let toCountry (country: Country) : External.Country =
        let result = External.Country()
        
        result.Name <- country.Name
        result.City <- toCity country.City
        
        result

    let toEmbassy (embassy: Embassy) : External.Embassy =
        let result = External.Embassy()
        
        result.Name <- embassy.Name
        result.Country <- toCountry embassy.Country
        
        result

    let toConfirmation (confirmation: Confirmation option) : External.Confirmation option =
        match confirmation with
        | None -> None
        | Some confirmation ->
            let result = External.Confirmation()
            result.Description <- confirmation.Description
            
            Some result

    let toAppointment (appointment: Appointment) : External.Appointment =
        let result = External.Appointment()

        result.Value <- appointment.Value
        result.DateTime <- appointment.Date.ToDateTime(appointment.Time)
        result.Description <- appointment.Description |> Option.defaultValue ""
        result.Confirmation <- appointment.Confirmation |> toConfirmation

        result

    let toRequest (request: Request) : External.Request =
        let result = External.Request()

        result.Id <- request.Id.Value
        result.Value <- request.Value
        result.Attempt <- request.Attempt
        result.State <- request.State.Name
        result.Embassy <- toEmbassy request.Embassy
        result.Appointments <- request.Appointments |> Seq.map toAppointment |> Seq.toArray
        result.Description <- request.Description |> Option.defaultValue ""
        result.Modified <- request.Modified

        result
