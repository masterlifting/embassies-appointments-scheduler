module KdmidScheduler.Core

open KdmidScheduler.Domain.Core

let getKdmidCredentials' city persistenceType =
    async {
        match Persistence.Scope.create persistenceType with
        | Error error -> return Error error
        | Ok scope ->

            Persistence.Scope.remove scope

            let kdmidCredentials =
                set
                    [ { Id = Id "1"
                        Cd = Cd "1"
                        Ems = Ems(Some "1") }
                      { Id = Id "1"
                        Cd = Cd "1"
                        Ems = Ems(Some "1") } ]

            return Ok kdmidCredentials
    }

let getKdmidCredentials city =
    getKdmidCredentials' city Persistence.InMemoryStorage
