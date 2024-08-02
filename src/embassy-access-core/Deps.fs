[<RequireQualifiedAccess>]
module EmbassyAccess.Deps

module Russian =
    let getAppointments (storage,ct)  = Embassies.Russian.Deps.createGetAppointmentsDeps ct storage |> Api.GetAppointmentsDeps.Russian
    let bookAppointment (storage,ct) = Embassies.Russian.Deps.createBookAppointmentDeps ct storage |> Api.BookAppointmentDeps.Russian 