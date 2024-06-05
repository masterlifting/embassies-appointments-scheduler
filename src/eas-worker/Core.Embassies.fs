module internal Eas.Worker.Core.Embassies

open Infrastructure.Domain.Graph
open Worker.Domain.Core
open Eas.Domain.Internal.Core

module Russian =

    let private getAvailableDates country =
        fun ct ->
            async {

                let getEmbassyResponse = Eas.Api.createGetEmbassyResponse None

                let getAvailableDates () =
                    getEmbassyResponse { Embassy = Russian country; Data = "" } ct

                match! getAvailableDates () with
                | Error error -> return Error error
                | Ok None -> return Ok <| Info "No data available"
                | Ok(Some result) -> return Ok <| Data result.Appointments
            }

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
