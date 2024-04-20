module KdmidScheduler.Core

open KdmidScheduler.Domain.Core

let getUserCredentials' city persistenceType =
    async {
        match Persistence.Scope.create persistenceType with
        | Error error -> return Error error
        | Ok scope ->

            Persistence.Scope.remove scope

            match Kdmid.createCredentials (Kdmid.Id "1") (Kdmid.Cd "1") (Kdmid.Ems(Some "1")) with
            | Error error -> return Error error
            | Ok kdmidCredentials ->

                let userCredentials =
                    Map
                        [ { Id = UserId "1"; Name = "John" }, Set [ kdmidCredentials ]
                          { Id = UserId "2"; Name = "Jane" }, Set [ kdmidCredentials ] ]


                return Ok userCredentials
    }

let getUserCredentials city =
    getUserCredentials' city Persistence.InMemoryStorage


let getAvailableDates cityOrder : Async<Result<CityOrderResult, string>> =
    async
        {

        }
