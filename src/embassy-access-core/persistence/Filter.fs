[<RequireQualifiedAccess>]
module EmbassyAccess.Persistence.Filter

open System
open EmbassyAccess

type OrderBy<'a> =
    | Date of ('a -> DateTime)
    | String of ('a -> string)
    | Int of ('a -> int)
    | Bool of ('a -> bool)
    | Guid of ('a -> Guid)

type SortBy<'a> =
    | Asc of OrderBy<'a>
    | Desc of OrderBy<'a>

type Predicate<'a> = 'a -> bool

type Pagination<'a> =
    { Page: int
      PageSize: int
      SortBy: SortBy<'a> }

type Request =
    { Pagination: Pagination<Domain.Request> option
      Ids: Domain.RequestId list
      Embassy: Domain.Embassy option
      Modified: Predicate<DateTime> option }

type AppointmentsResponse =
    { Pagination: Pagination<Domain.AppointmentsResponse> option
      Ids: Domain.ResponseId list
      Request: Domain.Request option
      Modified: Predicate<DateTime> option }

type ConfirmationResponse =
    { Pagination: Pagination<Domain.ConfirmationResponse> option
      Ids: Domain.ResponseId list
      Request: Domain.Request option
      Modified: Predicate<DateTime> option }
