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
            |> ResultAsync.wrap (fun storage ->

                let setEmbassyRequest = Eas.Api.Set.initSetUserEmbassyRequest <| Some storage

                let user: User = {Name = "Andrei"}
                let request = {Embassy = Russian country; Data = "https://belgrad.kdmid.ru/queue/"}

                let setRequest () =
                    setEmbassyRequest user request ct

                let getEmbassyRequests = Eas.Api.Get.initGetEmbassyRequests <| Some storage

                let getEmbassyResponse = Eas.Api.Get.initGetEmbassyResponse <| Some storage

                let getResponse request =
                    getEmbassyResponse
                        { Embassy = Russian country
                          Data = request }

                let attempts = 3

                async {
                    let! a = setRequest ()

                    match! getEmbassyRequests (Russian country) ct with
                    | Error error -> return Error error
                    | Ok requests ->
                        let urls = requests |> List.map (fun x -> x.Data)

                        match! tryGetResponse urls attempts ct getResponse with
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
