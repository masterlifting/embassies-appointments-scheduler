module Eas.Mapper

open Infrastructure.DSL.ActivePatterns
open Infrastructure.Domain.Errors

let mapToInternalEmbassy (embassy: Domain.External.Embassy) : Result<Domain.Internal.Embassy, InfrastructureError> =
    match embassy.Name with
    | IsString "Russian" -> Ok <| Domain.Internal.Embassy.Russian
    | _ -> Error <| Mapping "Embassy not supported"

let mapToExternalEmbassy (embassy: Domain.Internal.Embassy) : Domain.External.Embassy =
    match embassy with
    | Domain.Internal.Embassy.Russian -> { Name = "Russian" }

let mapToInternalCountry (country: Domain.External.Country) : Result<Domain.Internal.Country, InfrastructureError> =
    match country.Name with
    | IsString "Serbia" -> Ok <| Domain.Internal.Country.Serbia
    | IsString "Bosnia" -> Ok <| Domain.Internal.Country.Bosnia
    | IsString "Montenegro" -> Ok <| Domain.Internal.Country.Montenegro
    | IsString "Albania" -> Ok <| Domain.Internal.Country.Albania
    | IsString "Hungary" -> Ok <| Domain.Internal.Country.Hungary
    | _ -> Error <| Mapping "Country not supported"

let mapToExternalCountry (country: Domain.Internal.Country) : Domain.External.Country =
    match country with
    | Domain.Internal.Country.Serbia -> { Name = "Serbia" }
    | Domain.Internal.Country.Bosnia -> { Name = "Bosnia" }
    | Domain.Internal.Country.Montenegro -> { Name = "Montenegro" }
    | Domain.Internal.Country.Albania -> { Name = "Albania" }
    | Domain.Internal.Country.Hungary -> { Name = "Hungary" }

let mapToInternalCity (city: Domain.External.City) : Result<Domain.Internal.City, InfrastructureError> =
    match city.Name with
    | IsString "Belgrade" -> Ok <| Domain.Internal.City.Belgrade
    | IsString "Budapest" -> Ok <| Domain.Internal.City.Budapest
    | IsString "Sarajevo" -> Ok <| Domain.Internal.City.Sarajevo
    | IsString "Podgorica" -> Ok <| Domain.Internal.City.Podgorica
    | IsString "Tirana" -> Ok <| Domain.Internal.City.Tirana
    | IsString "Paris" -> Ok <| Domain.Internal.City.Paris
    | IsString "Rome" -> Ok <| Domain.Internal.City.Rome
    | _ -> Error <| Mapping "City not supported"

let mapToExternalCity (city: Domain.Internal.City) : Domain.External.City =
    match city with
    | Domain.Internal.City.Belgrade -> { Name = "Belgrade" }
    | Domain.Internal.City.Budapest -> { Name = "Budapest" }
    | Domain.Internal.City.Sarajevo -> { Name = "Sarajevo" }
    | Domain.Internal.City.Podgorica -> { Name = "Podgorica" }
    | Domain.Internal.City.Tirana -> { Name = "Tirana" }
    | Domain.Internal.City.Paris -> { Name = "Paris" }
    | Domain.Internal.City.Rome -> { Name = "Rome" }
