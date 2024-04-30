module KdmidScheduler.Core

let getOrderResults city credentials =

    let rec innerLoop city credentials error =
        async {
            match credentials with
            | [] ->
                match error with
                | None -> return Ok None
                | Some error -> return Error error
            | credential :: credentialsTail ->
                match! Web.Http.getKdmidOrderResults city credential with
                | Error error -> return! innerLoop city credentialsTail (Some error)
                | Ok None -> return Ok None
                | Ok(Some orderResults) -> return Ok <| Some orderResults
        }

    innerLoop city credentials None
let getUserCredentials = Persistence.Repository.UserCredentials.get
let createTestUserCredentials = Persistence.Repository.UserCredentials.createTest
