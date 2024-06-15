module Eas.Api

open Infrastructure.Domain.Errors
open Infrastructure.Dsl
open Eas.Domain.Internal
open Eas.Persistence

let getSupportedEmbassies () =
    Set
    <| [ Russian <| Serbia Belgrade
         Russian <| Bosnia Sarajevo
         Russian <| Hungary Budapest
         Russian <| Montenegro Podgorica
         Russian <| Albania Tirana ]
