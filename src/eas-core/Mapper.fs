module Eas.Mapper

open Infrastructure.Domain.Errors
open Eas.Domain

module Internal =

    let toCity (city: External.City) =
        match city.Name with
        | "Belgrade" -> Ok <| Internal.Belgrade
        | "Sarajevo" -> Ok <| Internal.Sarajevo
        | "Budapest" -> Ok <| Internal.Budapest
        | "Podgorica" -> Ok <| Internal.Podgorica
        | "Tirana" -> Ok <| Internal.Tirana
        | "Paris" -> Ok <| Internal.Paris
        | "Rome" -> Ok <| Internal.Rome
        | _ -> Error <| (Mapping $"City {city.Name} not supported.")

    let toCountry (country: External.Country) =
        toCity country.City
        |> Result.bind (fun city ->
            match country.Name with
            | "Serbia" -> Ok <| Internal.Serbia city
            | "Bosnia" -> Ok <| Internal.Bosnia city
            | "Hungary" -> Ok <| Internal.Hungary city
            | "Montenegro" -> Ok <| Internal.Montenegro city
            | "Albania" -> Ok <| Internal.Albania city
            | _ -> Error <| (Mapping $"Country {country.Name} not supported."))

    let toEmbassy (embassy: External.Embassy) =
        toCountry embassy.Country
        |> Result.bind (fun country ->
            match embassy.Name with
            | "Russian" -> Ok <| Internal.Russian country
            | _ -> Error <| (Mapping $"Embassy {embassy.Name} not supported."))


    let toRequest (request: External.Request) : Result<Internal.Request, InfrastructureError> =
        toEmbassy request.Embassy
        |> Result.bind (fun embassy ->
            Ok
                { Embassy = embassy
                  Data = request.Data |> Array.map (fun x -> x.Key, x.Value) |> Map.ofArray
                  Modified = request.Modified })

module External =

    let toCity (city: Internal.City) =
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

    let toCountry (country: Internal.Country) =
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

    let toEmbassy (embassy: Internal.Embassy) =
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

    let toRequest (user: Internal.User) (request: Internal.Request) =
        let result = new External.Request()

        result.Data <-
            request.Data
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                let data = new External.RequestData()
                data.Key <- key
                data.Value <- value
                data)
            |> Seq.toArray

        result.User <- new External.User()
        result.User.Name <- user.Name

        result.Embassy <- toEmbassy request.Embassy
        result.Modified <- request.Modified

        result
