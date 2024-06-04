module Eas.Api

open Infrastructure.Domain.Errors
open Eas.Domain.Internal.Core

let getEmbassies () =
    async { return Ok <| set [ Russian; Spanish; Italian; French; German; British ] }

let getEmbassyCountries embassy =
    async {
        return
            match embassy with
            | Russian -> Ok <| set [ Serbia; Bosnia; Montenegro; Albania; Hungary ]
            | _ -> Error <| Logical NotImplemented
    }

let getEmbassyCountryCities embassy country =
    async {
        return
            match embassy, country with
            | Russian, Serbia -> Ok <| set [ Belgrade ]
            | Russian, Bosnia -> Ok <| set [ Sarajevo ]
            | Russian, Montenegro -> Ok <| set [ Podgorica ]
            | Russian, Albania -> Ok <| set [ Tirana ]
            | Russian, Hungary -> Ok <| set [ Budapest ]
            | _ -> Error <| Logical NotImplemented
    }

let setEmbassyRequest storage =
    let storageRes =
        match storage with
        | Some storage -> Ok storage
        | None -> Persistence.Repository.getMemoryStorage ()

    fun embassy country city request ct ->
        match storageRes with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match embassy with
            | Russian -> Core.Russian.setCredentials request storage ct
            | _ -> async { return Error <| Logical NotImplemented }

let getEmbassyAppointments embassy request ct =
    match embassy with
    | Russian -> Core.Russian.getAppointments request ct
    | _ -> async { return Error <| Logical NotImplemented }

let setEmbassyAppointment appointment ct : Async<Result<string, ApiError>> =
    async { return Error <| Logical NotImplemented }
