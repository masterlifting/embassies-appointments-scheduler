module KdmidScheduler.Core

open KdmidScheduler.Domain

let getKdmidCredentials' city persistenceType =
    async {
        match Persistence.Scope.create persistenceType with
        | Error error -> return Error error
        | Ok scope ->

            Persistence.Scope.remove scope

            return Ok ""
    }

let getKdmidCredentials city =
    getKdmidCredentials' city Persistence.InMemoryStorage
