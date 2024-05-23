module Eas.Core

//open Infrastructure.Domain
//open Infrastructure.Domain.Errors
//open KdmidScheduler.Domain.Core.Embassies

//module Kdmid =
//    open KdmidScheduler.Domain.Core.Kdmid

//    let getCredentialAppointments credentials =

//        let rec innerLoop credentials error =
//            async {
//                match credentials, error with
//                | [], None -> return Ok None
//                | [], Some error -> return Error error
//                | credentialsHead :: credentialsTail, _ ->
//                    match! Web.Http.getKdmidAppointmentsResult credentialsHead with
//                    | Error(InvalidRequest error) -> return Error error
//                    | Error(InvalidResponse error) -> return! innerLoop credentialsTail (Some error)
//                    | Error(InvalidCredentials error) -> return! innerLoop credentialsTail (Some error)
//                    | Ok appointmentsSet when appointmentsSet.IsEmpty -> return Ok None
//                    | Ok appointmentsSet ->

//                        let result =
//                            { Credentials = credentialsHead
//                              Appointments = appointmentsSet }

//                        match! innerLoop credentialsTail error with
//                        | Error error -> return Error error
//                        | Ok None -> return Ok <| Some [ result ]
//                        | Ok(Some next) -> return Ok <| Some(result :: next)
//            }

//        let credentialsList = credentials |> Set.toList
//        innerLoop credentialsList None

//module User =
//    let getUserKdmidOrders = Persistence.Repository.User.getKdmidOrders
//    let createKdmidOrder = Persistence.Repository.User.createKdmidOrder

//let processEmbassy (embassy: Embassy) : Async<Result<AppointmentResult, AppError>> =
//    async {
//        return
//            match embassy with
//            | Russian russian ->
//                match russian with
//                | Serbia serbia ->
//                    match serbia with
//                    | Belgrade -> Error <| LogicError NotImplemented
//                    | _ -> Error <| LogicError NotSupported
//                | _ -> Error <| LogicError NotSupported
//            | _ -> Error <| LogicError NotSupported
//    }
