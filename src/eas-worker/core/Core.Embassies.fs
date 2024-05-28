module internal Eas.Worker.Core.Embassies

open System.Threading
open Infrastructure.DSL.Threading
open Infrastructure.Domain.Graph
open Infrastructure.Domain.Errors
open Worker.Domain.Core

module Russian =
    let private getAvailableDates city =
        fun (ct: CancellationToken) ->
            async {

                if ct |> canceled then
                    return Error <| Logical(Cancelled "GetAvailableDates")
                else
                    return Ok city
            }

    let private notifyUsers city = fun ct -> async { return Ok city }

    let createNode city =
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
