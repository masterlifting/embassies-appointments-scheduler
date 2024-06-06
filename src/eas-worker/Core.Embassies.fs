﻿module internal Eas.Worker.Core.Embassies

open Infrastructure.DSL
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Worker.Domain.Core
open Eas.Domain.Internal.Core

module Russian =
    open Persistence

    let rec private tryGetAvailableDates credentials attempts ct getAppointments =
        async {
            match credentials with
            | [] -> return Ok None
            | head :: tail ->
                match! getAppointments head ct with
                | Ok None -> return Ok None
                | Ok(Some appointments) -> return Ok <| Some appointments
                | Error error ->
                    match error with
                    | Infrastructure(InvalidRequest _) ->
                        if attempts = 0 then
                            return Error error
                        else
                            return! tryGetAvailableDates tail (attempts - 1) ct getAppointments
                    | _ -> return Error error
        }

    let private getAvailableDates country =
        fun ct ->
            Core.createStorage Core.InMemory
            |> Result.mapError Infrastructure
            |> ResultAsync.bind (fun storage ->
                let getEmbassyResponse = Eas.Api.initGetEmbassyResponse <| Some storage
                
                let getCountryCredentials =
                    Eas.Api.initGetEmbassyCountryRequestData <| Some storage
                
                let getAvailableDates credentials ct =
                    getEmbassyResponse
                        { Embassy = Russian country
                          Data = credentials }
                        ct
                
                async {
                    match! getCountryCredentials (Russian country) ct with
                    | Error error -> return Error error
                    | Ok credentials ->
                        match! tryGetAvailableDates credentials 3 ct getAvailableDates with
                        | Error error -> return Error error
                        | Ok None -> return Ok <| Info "No data available"
                        | Ok(Some result) -> return  Ok <| Data result.Appointments
                })

    let createStepsFor country =
        Node(
            { Name = "RussianEmbassy"
              Handle = None },
            [ Node(
                  { Name = "GetAvailableDates"
                    Handle = Some <| getAvailableDates country },
                  []
              ) ]
        )
