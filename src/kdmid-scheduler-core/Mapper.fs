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
                            { Id = entity.Id |> Core.KdmidCredentialId.Id
                              Cd = entity.Cd |> Core.KdmidCredentialCd.Cd
                              Ems = None |> Core.KdmidCredentialEms.Ems }
                    | false ->
                        Ok
                            { Id = entity.Id |> Core.KdmidCredentialId.Id
                              Cd = entity.Cd |> Core.KdmidCredentialCd.Cd
                              Ems = Some entity.Ems |> Core.KdmidCredentialEms.Ems }

        let toPersistence (model: Core.KdmidCredentials) : Persistence.KdmidCredentials =
            { Id =
                match model.Id with
                | Core.KdmidCredentialId.Id id -> id
              Cd =
                match model.Cd with
                | Core.KdmidCredentialCd.Cd cd -> cd
              Ems =
                match model.Ems with
                | Core.KdmidCredentialEms.Ems ems ->
                    match ems with
                    | None -> System.String.Empty
                    | Some ems -> ems }
