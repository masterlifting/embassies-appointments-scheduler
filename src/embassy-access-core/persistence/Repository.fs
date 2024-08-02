[<RequireQualifiedAccess>]
module EmbassyAccess.Persistence.Repository

open Infrastructure
open Persistence
open EmbassyAccess.Persistence

module Query =

    module Request =

        let get ct filter storage =
            match storage with
            | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.Request.get ct filter
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let get' ct requestId storage =
            match storage with
            | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.Request.get' ct requestId
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

    module AppointmentsResponse =

        let get ct filter storage =
            match storage with
            | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.AppointmentsResponse.get ct filter
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let get' ct responseId storage =
            match storage with
            | Storage.Type.InMemory context ->
                context |> InMemoryRepository.Query.AppointmentsResponse.get' ct responseId
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

    module ConfirmationResponse =

        let get ct filter storage =
            match storage with
            | Storage.Type.InMemory context -> context |> InMemoryRepository.Query.ConfirmationResponse.get ct filter
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let get' ct responseId storage =
            match storage with
            | Storage.Type.InMemory context ->
                context |> InMemoryRepository.Query.ConfirmationResponse.get' ct responseId
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

module Command =

    module Request =

        let private execute ct command storage =
            match storage with
            | Storage.Type.InMemory context -> context |> InMemoryRepository.Command.Request.execute ct command
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let create ct request =
            Command.Request.Create request |> execute ct

        let update ct request =
            Command.Request.Update request |> execute ct

        let delete ct request =
            Command.Request.Delete request |> execute ct

    module AppointmentsResponse =

        let private execute ct command storage =
            match storage with
            | Storage.Type.InMemory context ->
                context |> InMemoryRepository.Command.AppointmentsResponse.execute ct command
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let create ct response =
            Command.AppointmentsResponse.Create response |> execute ct

        let update ct response =
            Command.AppointmentsResponse.Update response |> execute ct

        let delete ct response =
            Command.AppointmentsResponse.Delete response |> execute ct

    module ConfirmationResponse =

        let private execute ct command storage =
            match storage with
            | Storage.Type.InMemory context ->
                context |> InMemoryRepository.Command.ConfirmationResponse.execute ct command
            | _ -> async { return Error <| NotSupported $"Storage {storage}" }

        let create ct response =
            Command.ConfirmationResponse.Create response |> execute ct

        let update ct response =
            Command.ConfirmationResponse.Update response |> execute ct

        let delete ct response =
            Command.ConfirmationResponse.Delete response |> execute ct
