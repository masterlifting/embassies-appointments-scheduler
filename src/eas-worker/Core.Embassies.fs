module internal Eas.Worker.Core.Embassies

open Infrastructure.DSL
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Worker.Domain.Internal
open Eas.Domain.Internal.Core

module Russian =
    let rec private tryGetResponse requests attempts ct getResponse =
        async {
            match requests with
            | [] -> return Ok None
            | request :: requestsTail ->
                match! getResponse request ct with
                | Error(Infrastructure(InvalidRequest error)) ->
                    match attempts with
                    | 0 -> return Error <| (Logical <| Cancelled error)
                    | _ -> return! tryGetResponse requestsTail (attempts - 1) ct getResponse
                | Error error -> return Error error
                | response -> return response
        }

    let private getAvailableDates country =
        fun ct ->
            Persistence.Core.createStorage Persistence.Core.InMemory
            |> Result.mapError Infrastructure
            |> ResultAsync.bind (fun storage ->

                let getCredentials = Eas.Api.Get.initGetEmbassyCountryRequests <| Some storage

                let getEmbassyResponse = Eas.Api.Get.initGetEmbassyResponse <| Some storage

                let getResponse credentials =
                    getEmbassyResponse
                        { Embassy = Russian country
                          Data = credentials }

                let attempts = 3

                async {
                    match! getCredentials (Russian country) ct with
                    | Error error -> return Error error
                    | Ok credentials ->
                        let credentials = credentials |> List.ofSeq
                        match! tryGetResponse credentials attempts ct getResponse with
                        | Error error -> return Error error
                        | Ok None -> return Ok <| Info "No data"
                        | Ok(Some response) ->
                            let setEmbassyResponse = Eas.Api.Set.initSetEmbassyResponse <| Some storage

                            match! setEmbassyResponse response ct with
                            | Error error -> return Error error
                            | _ -> return Ok <| Data response.Appointments
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
