module internal Eas.Worker.Embassies

open Infrastructure.Dsl
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Persistence.Domain
open Worker.Domain.Internal
open Eas.Domain.Internal
open Eas.Persistence.QueryFilter

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

                let requestFilter =
                    { Pagination = { Page = 1; PageSize = 10 }
                      Embassy = Russian country }

                let getResponse request ct =
                    Eas.Api.Get.initGetEmbassyResponse storage request ct

                let attempts = 3

                async {

                    //TODO: remove this block after the development
                    let request =
                        { Id = System.Guid.NewGuid() |> RequestId
                          User = { Id = UserId 0; Name = "Andrei" }
                          Embassy = Russian country
                          Data =
                            Map
                                [ "url",
                                  "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                          Modified = System.DateTime.UtcNow }

                    let! _ = Eas.Api.addRequest storage request ct //TODO: remove this line after the development
                    //

                    match! getRequests <| Russian country with
                    | Error error -> return Error error
                    | Ok requests ->
                        match! tryGetEmbassyResponse requests attempts ct getResponse with
                        | Error error -> return Error error
                        | Ok None -> return Ok <| Info "No data"
                        | Ok(Some response) ->
                            let setEmbassyResponse = Eas.Api.setEmbassyResponse response

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
