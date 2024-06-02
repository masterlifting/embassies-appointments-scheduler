﻿module internal Eas.Worker.Core.Embassies

open Infrastructure.Domain.Graph
open Worker.Domain.Core

module Russian =

    let private getAvailableDates city =
        fun ct ->
            async {
                match! Eas.Core.Russian.findAppointments city ct 3 with
                | Error error -> return Error error
                | Ok None -> return Ok <| Info "No data available"
                | Ok(Some result) -> return Ok <| Data result
            }

    let private notifyUsers city =
        fun ct ->
            async {
                match! Eas.Core.Russian.notifyUsers city ct with
                | Ok(Some result) -> return Ok <| Data result
                | Ok None -> return Ok <| Info "No data to notify"
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