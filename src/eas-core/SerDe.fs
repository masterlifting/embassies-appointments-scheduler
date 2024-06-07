module internal Eas.SerDe

module Json =
   open Infrastructure.DSL
   open Domain
   open Mapper

   module Russian =
         open System
         let serializeCredentials (credentials: string) : External.Russian.Credentials =
             let credentials = SerDe.Json.deserialize<External.Russian.Credentials> credentials

