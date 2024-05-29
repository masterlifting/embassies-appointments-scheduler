module Eas.Core

open System.Threading
open Eas.Domain.Core.Embassies

module Embassies =
    module Russian =
        let getAvailableDates (city: City) (ct: CancellationToken) =
            async {
                return Ok <| Some city
            }

        let notifyUsers (city: City) (ct: CancellationToken) =
            async {
                return Ok <| None
            }