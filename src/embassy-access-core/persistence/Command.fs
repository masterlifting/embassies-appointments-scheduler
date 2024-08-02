[<RequireQualifiedAccess>]
module EmbassyAccess.Persistence.Command

open EmbassyAccess

type Request =
    | Create of Domain.Request
    | Update of Domain.Request
    | Delete of Domain.Request

type AppointmentsResponse =
    | Create of Domain.AppointmentsResponse
    | Update of Domain.AppointmentsResponse
    | Delete of Domain.AppointmentsResponse

type ConfirmationResponse =
    | Create of Domain.ConfirmationResponse
    | Update of Domain.ConfirmationResponse
    | Delete of Domain.ConfirmationResponse
