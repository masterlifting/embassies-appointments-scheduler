[<RequireQualifiedAccess>]
module EmbassyAccess.Api

open Infrastructure
open EmbassyAccess.Domain

type GetAppointmentsDeps = Russian of Embassies.Russian.Domain.GetAppointmentsDeps

type BookAppointmentApiDeps = Russian of Embassies.Russian.Domain.BookAppointmentDeps

let getAppointments deps request =
    match deps with
    | GetAppointmentsDeps.Russian deps -> request |> Embassies.Russian.Api.getAppointments deps

let tryGetAppointments getAppointments =
    let rec innerLoop (requests: Request list) (errors: Error' list option) attempt =
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
                                |> List.mapi (fun i error -> $"{i+1}.{error.Message}")
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

                    let attempt = attempt + 1u
                    return! innerLoop requestsTail errors attempt
                | response -> return response
        }

    fun requests -> innerLoop requests None 1u

let bookAppointment deps option request =
    match deps with
    | BookAppointmentApiDeps.Russian deps -> request |> Embassies.Russian.Api.bookAppointment deps option
