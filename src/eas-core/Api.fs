module Eas.Api

open System.Threading
open Infrastructure.Domain.Errors
open Eas.Domain.Internal.Core


let getSupportedEmbassies () =
    Set
    <| [ Russian <| Serbia Belgrade
         Russian <| Bosnia Sarajevo
         Russian <| Hungary Budapest
         Russian <| Montenegro Podgorica
         Russian <| Albania Tirana ]

let createGetEmbassyUserRequestData storage =
    fun user embassy ct ->
        match Persistence.Repository.getStorage storage with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match embassy with
            | Russian country -> Core.Russian.getUserCredentials storage user country ct
            | _ -> async { return Error <| Logical NotSupported }

let createGetEmbassyCountryRequestData storage =
    fun embassy ct ->
        match Persistence.Repository.getStorage storage with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match embassy with
            | Russian country -> Core.Russian.getCountryCredentials storage country ct
            | _ -> async { return Error <| Logical NotSupported }

let createSetEmbassyRequestData storage =
    fun user embassy credentials ct ->
        match Persistence.Repository.getStorage storage with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match embassy with
            | Russian country -> Core.Russian.setCredentials storage user country credentials ct
            | _ -> async { return Error <| Logical NotSupported }

let createGetEmbassyResponse storage =
    fun (request: Request) (ct: CancellationToken) ->
        match Persistence.Repository.getStorage storage with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match request.Embassy with
            | Russian _ -> Core.Russian.getEmbassyResponse request storage ct
            | _ -> async { return Error <| Logical NotSupported }

let createSetEmbassyResponse storage =
    fun (response: Response) (ct: CancellationToken) ->
        match Persistence.Repository.getStorage storage with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match response.Embassy with
            | Russian _ -> Core.Russian.setEmbassyResponse response storage ct
            | _ -> async { return Error <| Logical NotSupported }
