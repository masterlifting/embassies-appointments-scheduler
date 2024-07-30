[<RequireQualifiedAccess>]
module EmbassyAccess.Api

type GetAppointmentsParams = Russian of Embassies.Russian.Domain.GetAppointmentsDeps

type TryGetAppointmentsParams = Russian of Embassies.Russian.Domain.TryGetAppointments

type BookAppointmentParams = Russian of Embassies.Russian.Domain.BookAppointmentDeps

let getAppointments args request =
    match args with
    | GetAppointmentsParams.Russian deps -> request |> Embassies.Russian.Api.getAppointments deps

let tryGetAppointments args requests =
    match args with
    | TryGetAppointmentsParams.Russian deps -> requests |> Embassies.Russian.Api.tryGetAppointments deps

let bookAppointment args option request =
    match args with
    | BookAppointmentParams.Russian deps -> Embassies.Russian.Api.bookAppointment deps request option
