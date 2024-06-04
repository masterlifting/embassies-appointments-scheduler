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

let createSetEmbassyRequest storage =
    let storageRes =
        match storage with
        | Some storage -> Ok storage
        | None -> Persistence.Repository.getMemoryStorage ()

    fun (request: Request) (ct: CancellationToken) ->
        match storageRes with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match request.Embassy with
            | Russian _ -> Core.Russian.setCredentials request storage ct
            | _ -> async { return Error <| Logical NotImplemented }

let createGetEmbassyResponse storage =
    let storageRes =
        match storage with
        | Some storage -> Ok storage
        | None -> Persistence.Repository.getMemoryStorage ()

    fun (request: Request) (ct: CancellationToken) ->
        match request.Embassy with
        | Russian _ -> Core.Russian.getAppointments request storage ct
        | _ -> async { return Error <| Logical NotImplemented }

let createSetEmbassyResponse storage =
    let storageRes =
        match storage with
        | Some storage -> Ok storage
        | None -> Persistence.Repository.getMemoryStorage ()

    fun (response: Response) (ct: CancellationToken) ->
        match storageRes with
        | Error error -> async { return Error <| Infrastructure error }
        | Ok storage ->
            match response.Embassy with
            | Russian _ -> Core.Russian.confirmAppointment response storage ct
            | _ -> async { return Error <| Logical NotImplemented }
