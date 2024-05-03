module KdmidScheduler.Core

let private getKdmidOrderResults city credentials =

    let rec innerLoop credentials city error =
        async {
            match credentials, error with
            | [], None -> return Ok None
            | [], Some error -> return Error error
            | credential :: credentialsTail, _ ->
                match! Web.Http.getKdmidOrderResults city credential with
                | Error error -> return! innerLoop credentialsTail city (Some error)
                | Ok None -> return Ok None
                | Ok(Some orderResults) ->
                    let! tailOrderResults =
                        credentialsTail
                        |> Seq.map (fun x -> Web.Http.getKdmidOrderResults city x)
                        |> Async.Parallel

                    let result = set [ tailOrderResults, Some orderResults ]

                    return Ok <| Some(credential, orderResults)
        }

    let credentialsList = credentials |> Seq.toList
    innerLoop credentialsList city None

let private handleCities user (cityCredentials: Domain.Core.CityCredentials) =
    cityCredentials
    |> Seq.map (fun x -> getKdmidOrderResults x.Key x.Value)
    |> Async.Parallel

let private handleUsers (userCredentials: Domain.Core.UserCredentials) =
    userCredentials
    |> Seq.map (fun x -> handleCities x.Key x.Value)
    |> Async.Parallel

let getOrderResults = handleUsers
let getUserCredentials = Persistence.Repository.UserCredentials.get
let createTestUserCredentials = Persistence.Repository.UserCredentials.createTest
