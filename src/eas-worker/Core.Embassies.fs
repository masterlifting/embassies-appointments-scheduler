module internal Eas.Worker.Core.Embassies

open Infrastructure.Domain.Errors
open Infrastructure.Domain.Graph
open Worker.Domain.Core

module Russian =

    let private getAvailableDates city =
        fun ct ->
            async {
                match! Eas.Core.Russian.getAvailableDates city ct with
                | Ok(Some result) -> return Ok <| Data result
                | Ok None -> return Ok <| Info "No available dates"
                | Error error -> return Error error
            }

    let private notifyUsers city =
        fun ct ->
            async {
                match! Eas.Core.Russian.notifyUsers city ct with
                | Ok(Some result) -> return Ok <| Data result
                | Ok None -> return Ok <| Info "No users to notify"
                | Error error -> return Error error
            }

    let createStepsFor city =
        Node(
            { Name = "RussianEmbassy"
              Handle = None },
            [ Node(
                  { Name = "GetAvailableDates"
                    Handle = Some <| getAvailableDates city },
                  []
              )
              Node(
                  { Name = "NotifyUsers"
                    Handle = Some <| notifyUsers city },
                  []
              ) ]
        )
