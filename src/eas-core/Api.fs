module Eas.Api

open Infrastructure.DSL
open Infrastructure.Domain.Errors
open Eas.Domain.Internal.Core

let getSupportedEmbassies () =
    Set
    <| [ Russian <| Serbia Belgrade
         Russian <| Bosnia Sarajevo
         Russian <| Hungary Budapest
         Russian <| Montenegro Podgorica
         Russian <| Albania Tirana ]

let initGetEmbassyUserRequestData storage =
    fun user embassy ct ->
        Persistence.Repository.getStorage storage
        |> Result.mapError Infrastructure
        |> ResultAsync.bind (fun storage ->
            match embassy with
            | Russian country -> Core.Russian.getUserCredentials storage user country ct
            | _ -> async { return Error <| Logical NotSupported })

let initGetEmbassyCountryRequestData storageOpt =
    fun embassy ct ->
        storageOpt
        |> Persistence.Repository.getStorage
        |> Result.mapError Infrastructure
        |> ResultAsync.bind (fun storage ->
            match embassy with
            | Russian country -> Core.Russian.getCountryCredentials storage country ct
            | _ -> async { return Error <| Logical NotSupported })

let initSetEmbassyRequestData storageOpt =
    fun user embassy credentials ct ->
        storageOpt
        |> Persistence.Repository.getStorage
        |> Result.mapError Infrastructure
        |> ResultAsync.bind (fun storage ->
            match embassy with
            | Russian country -> Core.Russian.setCredentials storage user country credentials ct
            | _ -> async { return Error <| Logical NotSupported })

let initGetEmbassyResponse storageOpt =
    fun (request: Request) ct ->
        storageOpt
        |> Persistence.Repository.getStorage
        |> Result.mapError Infrastructure
        |> ResultAsync.bind (fun storage ->
            match request.Embassy with
            | Russian _ -> Core.Russian.getEmbassyResponse request storage ct
            | _ -> async { return Error <| Logical NotSupported })

let initSetEmbassyResponse storageOpt =
    fun (response: Response) ct ->
        storageOpt
        |> Persistence.Repository.getStorage
        |> Result.mapError Infrastructure
        |> ResultAsync.bind (fun storage ->
            match response.Embassy with
            | Russian _ -> Core.Russian.setEmbassyResponse response storage ct
            | _ -> async { return Error <| Logical NotSupported })
