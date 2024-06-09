module internal Eas.Persistence

open System.Threading
open Infrastructure
open Infrastructure.DSL.SerDe
open Infrastructure.DSL.Threading
open Infrastructure.Domain.Errors
open Persistence.Core
open Domain

module private InMemoryRepository =

    let private getEmbassyRequests filter =
        let deserialize =
            Json.deserialize<External.Request array>
            |> Option.map
            >> Option.defaultValue (Ok [||])

        let map requests =
            match requests with
            | null -> []
            | [||] -> []
            | _ -> List.ofArray requests

        let filter requests = requests |> List.filter filter

        Result.bind (deserialize >> Result.map (map >> filter))

    module Get =

        let initGetEmbassyRequests (storage: Persistence.InMemory.Storage) =
            fun (embassy: Internal.Core.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            storage
                            |> Persistence.InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.Embassy.Name = embassy.Model.Name
                                && x.Embassy.Country.Name = embassy.Model.Country.Name
                                && x.Embassy.Country.City.Name = embassy.Model.Country.City.Name)
                        | _ -> Error <| Persistence "Operation canceled initGetUserEmbassyRequests"
                }

        let initGetUserEmbassyRequests (storage: Persistence.InMemory.Storage) =
            fun (user: Internal.Core.User) (embassy: Internal.Core.Embassy) (ct: CancellationToken) ->
                async {
                    let key = $"request-{embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->

                            storage
                            |> Persistence.InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.User.Name = user.Name
                                && x.Embassy.Name = embassy.Model.Name
                                && x.Embassy.Country.Name = embassy.Model.Country.Name
                                && x.Embassy.Country.City.Name = embassy.Model.Country.City.Name)
                        | _ -> Error <| Persistence "Operation canceled initGetUserRequests"
                }

    module Set =

        let initSetUserEmbassyRequest (storage: Persistence.InMemory.Storage) =
            fun (user: Internal.Core.User) (request: Internal.Core.Request) (ct: CancellationToken) ->
                async {
                    let key = $"request-{request.Embassy}"

                    return
                        match ct |> notCanceled with
                        | true ->
                            storage
                            |> Persistence.InMemory.get
                            |> fun get -> get key
                            |> getEmbassyRequests (fun x ->
                                x.User.Name = user.Name
                                && x.Embassy.Name = request.Embassy.Model.Name
                                && x.Embassy.Country.Name = request.Embassy.Model.Country.Name
                                && x.Embassy.Country.City.Name = request.Embassy.Model.Country.City.Name)
                            |> Result.bind (fun requests ->

                                let userDto = new External.User()
                                userDto.Name <- userDto.Name

                                let cityDto = new External.City()
                                cityDto.Name <- request.Embassy.Model.Country.City.Name

                                let countryDto = new External.Country()
                                countryDto.Name <- request.Embassy.Model.Country.Name
                                countryDto.City <- cityDto

                                let embassyDto = new External.Embassy()
                                embassyDto.Name <- request.Embassy.Model.Name
                                embassyDto.Country <- countryDto


                                let request = new External.Request()
                                request.Data <- request.Data
                                request.User <- userDto
                                request.Embassy <- embassyDto

                                match requests with
                                | [] ->
                                    [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        storage |> Persistence.InMemory.add |> (fun add -> add key value))
                                | _ ->
                                    requests
                                    |> List.append [ request ]
                                    |> Json.serialize
                                    |> Result.bind (fun value ->
                                        storage |> Persistence.InMemory.update |> (fun update -> update key value)))
                        | _ -> Error <| Persistence "Operation canceled initSetRequest"
                }

        let initSetEmbassyResponse (storage: Persistence.InMemory.Storage) =
            fun (response: Internal.Core.Response) (ct: CancellationToken) ->
                async {
                    let key = $"response-{response}"

                    return Error <| Persistence "Not implemented initSetEmbassyResponse"
                }

module Repository =

    let createStorage storage =
        match storage with
        | Some storage -> Ok storage
        | _ -> createStorage InMemory

    module Get =

        let initGetUserEmbassyRequests storage =
            fun user embassy ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetUserEmbassyRequests storage user embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetUserCredentials" }

        let initGetEmbassyRequests storage =
            fun embassy ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Get.initGetEmbassyRequests storage embassy ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initGetCountryCredentials" }

    module Set =

        let initSetUserEmbassyRequest storage =
            fun user request ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Set.initSetUserEmbassyRequest storage user request ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCredentials" }

        let initSetEmbassyResponse storage =
            fun response ct ->
                match storage with
                | MemoryStorage storage -> InMemoryRepository.Set.initSetEmbassyResponse storage response ct
                | _ -> async { return Error <| Persistence $"Not supported {storage} initSetCountryResponse" }
