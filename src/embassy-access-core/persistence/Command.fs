[<RequireQualifiedAccess>]
module EmbassyAccess.Persistence.Command

open EmbassyAccess

type Request =
    | Create of Domain.Request
    | Update of Domain.Request
    | Delete of Domain.Request
