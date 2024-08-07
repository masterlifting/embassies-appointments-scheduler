﻿[<RequireQualifiedAccess>]
module EmbassyAccess.Api

type GetAppointmentsDeps = Russian of Embassies.Russian.Domain.GetAppointmentsDeps

type BookAppointmentDeps = Russian of Embassies.Russian.Domain.BookAppointmentDeps


let getAppointments deps request =
    match deps with
    | GetAppointmentsDeps.Russian deps -> request |> Embassies.Russian.Core.getAppointments deps

let bookAppointment deps option request =
    match deps with
    | BookAppointmentDeps.Russian deps -> request |> Embassies.Russian.Core.bookAppointment deps option
