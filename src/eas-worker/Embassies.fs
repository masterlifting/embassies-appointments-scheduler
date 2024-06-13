module internal Eas.Worker.Embassies

open Infrastructure.Dsl
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Persistence.Domain
open Worker.Domain.Internal
open Eas.Domain.Internal

module Russian =
    let rec private tryGetEmbassyResponse requests attempts ct getResponse =
        async {
            match requests with
            | [] -> return Ok None
            | request :: requestsTail ->
                match! getResponse request ct with
                | Error(Infrastructure(InvalidRequest error)) ->
                    match attempts with
                    | 0 -> return Error(Logical(Cancelled error))
                    | _ -> return! tryGetEmbassyResponse requestsTail (attempts - 1) ct getResponse
                | Error error -> return Error error
                | response -> return response
        }

    let private getAvailableDates country =
        fun ct ->
            Persistence.Core.createStorage InMemory
            |> Result.mapError Infrastructure
            |> ResultAsync.wrap (fun storage ->

                let storage = Some storage

                let setEmbassyRequest user request =
                    Eas.Api.Set.initSetUserEmbassyRequest storage user request ct //TODO: remove this line after the development

                let getEmbassyRequests embassy =
                    Eas.Api.Get.initGetEmbassyRequests storage embassy ct

                let getEmbassyResponse request ct =
                    Eas.Api.Get.initGetEmbassyResponse storage request ct

                let attempts = 3

                async {

                    //TODO: remove this block after the development
                    let user: User = { Id = UserId 0; Name = "Andrei" }

                    let request =
                        { Id = System.Guid.NewGuid() |> RequestId
                          Embassy = Russian country
                          Data =
                            Map
                                [ "url",
                                  "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                          Modified = System.DateTime.UtcNow }

                    let! _ = setEmbassyRequest user request
                    //

                    match! getEmbassyRequests <| Russian country with
                    | Error error -> return Error error
                    | Ok requests ->
                        match! tryGetEmbassyResponse requests attempts ct getEmbassyResponse with
                        | Error error -> return Error error
                        | Ok None -> return Ok <| Info "No data"
                        | Ok(Some response) ->
                            let setEmbassyResponse = Eas.Api.Set.initSetEmbassyResponse storage

                            match! setEmbassyResponse response ct with
                            | Error error -> return Error error
                            | _ -> return Ok <| Success response.Appointments
                })

    let createNode country =
        Node(
            { Name = "RussianEmbassy"
              Handle = None },
            [ Node(
                  { Name = "GetAvailableDates"
                    Handle = Some <| getAvailableDates country },
                  []
              ) ]
        )
