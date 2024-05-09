module KdmidScheduler.Core

open KdmidScheduler.Domain.Core.Kdmid

let processKdmidOrder (order: Order) =

    let rec innerLoop credentials error =
        async {
            match credentials, error with
            | [], None -> return Ok None
            | [], Some error -> return Error error
            | credentialsHead :: credentialsTail, _ ->
                match! Web.Http.getKdmidOrderResults credentialsHead with
                | Error(InvalidRequest error) -> return Error error
                | Error(InvalidResponse error) -> return! innerLoop credentialsTail (Some error)
                | Error(InvalidCredentials error) -> return! innerLoop credentialsTail (Some error)
                | Ok resultSet when resultSet.IsEmpty -> return Ok None
                | Ok resultSet ->

                    let orderResult =
                        { Credentials = credentialsHead
                          Results = resultSet }
                        
                    match! innerLoop credentialsTail error with
                    | Error error -> return Error error
                    | Ok None -> return Ok <| Some [ orderResult ]
                    | Ok(Some next) -> return Ok <| Some(orderResult :: next)
        }

    let credentialsList = order |> Set.toList
    innerLoop credentialsList None
    
let processUserKdmidOrders orders =
    async {
        return Error "processUserKdmidOrders is not implemented."
    }

let getUserKdmidOrdersByCity = Persistence.Repository.getOrdersByCity
let createTestUserKdmidOrderForCity = Persistence.Repository.createTestOrder
