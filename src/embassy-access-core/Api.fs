[<RequireQualifiedAccess>]
module EmbassyAccess.Api

open Infrastructure
open EmbassyAccess.Domain

type GetAppointmentsDeps = Russian of Embassies.Russian.Domain.GetAppointmentsDeps

type BookAppointmentDeps = Russian of Embassies.Russian.Domain.BookAppointmentDeps


let getAppointments deps request =
    match deps with
    | GetAppointmentsDeps.Russian deps -> request |> Embassies.Russian.Api.getAppointments deps

let tryGetAppointments requests getAppointments =
    let rec innerLoop (requests: Request list) (errors: Error' list option) =
        async {
            match requests with
            | [] ->
                return
                    match errors with
                    | Some errors ->
                        match errors.Length with
                        | 1 -> Error errors[0]
                        | _ ->
                            let msg =
                                errors
                                |> List.mapi (fun i error -> $"{i + 1}.{error.Message}")
                                |> String.concat "\n"

                            let error =
                                Operation
                                    { Message = $"Multiple errors: \n{msg}"
                                      Code = None }

                            Error error
                    | None -> Ok None
            | request :: requestsTail ->
                match! getAppointments request with
                | Error requestError ->
                    let errors =
                        match errors with
                        | None -> [ requestError ]
                        | Some errors -> errors @ [ requestError ]
                        |> Some

                    return! innerLoop requestsTail errors
                | response -> return response
        }

    innerLoop requests None

let bookAppointment deps option request =
    match deps with
    | BookAppointmentDeps.Russian deps -> request |> Embassies.Russian.Api.bookAppointment deps option
