module KdmidScheduler.Core

open KdmidScheduler.Domain.Core

let getUserCredentials' city persistenceType =
    async {
        match Persistence.Scope.create persistenceType with
        | Error error -> return Error error
        | Ok scope ->

            Persistence.Scope.remove scope

            let userCredentials =
                Map
                    [ { Id = UserId "1"; Name = "John" },
                      Set
                          [ { Id = KdmidCredentialId "1"
                              Cd = KdmidCredentialCd "1"
                              Ems = KdmidCredentialEms(Some "1") }
                            { Id = KdmidCredentialId "2"
                              Cd = KdmidCredentialCd "2"
                              Ems = KdmidCredentialEms None } ]
                      { Id = UserId "2"; Name = "Jane" },
                      Set
                          [ { Id = KdmidCredentialId "3"
                              Cd = KdmidCredentialCd "3"
                              Ems = KdmidCredentialEms None }
                            { Id = KdmidCredentialId "4"
                              Cd = KdmidCredentialCd "4"
                              Ems = KdmidCredentialEms(Some "4") } ] ]


            return Ok userCredentials
    }

let getUserCredentials city =
    getUserCredentials' city Persistence.InMemoryStorage

let getAvailableDates cityOrder : Async<Result<CityOrderResult, string>> =
    async
        {

        }
