module KdmidScheduler.Core

let private handleKdmidCredentials user city credentials =

    let rec innerLoop city credentials error =
        async {
            match credentials, error with
            | [], None -> return Ok None
            | [], Some error -> return Error error
            | credentialsHead :: credentialsTail, _ ->
                match! Web.Http.getKdmidOrderResults city credentialsHead with
                | Error error -> return! innerLoop city credentialsTail (Some error)
                | Ok None -> return Ok None
                | Ok(Some orderResults) -> return Ok <| Some(user, city, orderResults)
        }

    let credentialsList = credentials |> Seq.toList
    innerLoop city credentialsList None

let private handleCityCredentials user (cityCredentials: Domain.Core.CityCredentials) =
    cityCredentials
    |> Seq.map (fun x -> handleKdmidCredentials user x.Key x.Value)
    |> Async.Parallel

let private handleUserCredentials (userCredentials: Domain.Core.UserCredentials) =
    userCredentials
    |> Seq.map (fun x -> handleCityCredentials x.Key x.Value)
    |> Async.Parallel

let getKdmidResults userCredentials =
    async {
        let! results = handleUserCredentials userCredentials
        return results |> Seq.collect id |> Infrastructure.DSL.Seq.resultOrError
    }

let getUserCredentials = Persistence.Repository.UserCredentials.get
let createTestUserCredentials = Persistence.Repository.UserCredentials.createTest
