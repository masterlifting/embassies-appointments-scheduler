module KdmidScheduler.Mapper

module Kdmid =
    module Credentials =
        open KdmidScheduler.Domain

        let fromPersistence (entity: Persistence.KdmidCredentials) : Result<Core.KdmidCredentials, string> =
            match System.String.IsNullOrWhiteSpace entity.Id with
            | true -> Error "Id is empty"
            | false ->
                match System.String.IsNullOrWhiteSpace entity.Cd with
                | true -> Error "Cd is empty"
                | false ->
                    match System.String.IsNullOrWhiteSpace entity.Ems with
                    | true ->
                        Ok
                            { Id = entity.Id |> Core.KdmidCredentialId
                              Cd = entity.Cd |> Core.KdmidCredentialCd
                              Ems = None |> Core.KdmidCredentialEms }
                    | false ->
                        Ok
                            { Id = entity.Id |> Core.KdmidCredentialId
                              Cd = entity.Cd |> Core.KdmidCredentialCd
                              Ems = Some entity.Ems |> Core.KdmidCredentialEms }

        let toPersistence (model: Core.KdmidCredentials) : Persistence.KdmidCredentials =
            { Id =
                match model.Id with
                | Core.KdmidCredentialId id -> id
              Cd =
                match model.Cd with
                | Core.KdmidCredentialCd cd -> cd
              Ems =
                match model.Ems with
                | Core.KdmidCredentialEms ems ->
                    match ems with
                    | None -> System.String.Empty
                    | Some ems -> ems }
