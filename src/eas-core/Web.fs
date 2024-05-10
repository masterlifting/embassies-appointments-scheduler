module internal KdmidScheduler.Web

module Http =
    open KdmidScheduler.Domain.Core.Kdmid
    open Domain.Core

    let private createUrlParams id cd ems =

        let id =
            match id with
            | PublicKdmidId id -> id

        let cd =
            match cd with
            | PublicKdmidCd cd -> cd

        let ems =
            match ems with
            | PublicKdmidEms ems -> ems

        match ems with
        | None -> $"id={id}&cd={cd}"
        | Some ems -> $"id={id}&cd={cd}&ems={ems}"

    let private createBaseUrl city =
        let city' =
            match city with
            | PublicCity city -> city

        $"https://{city'}.kdmid.ru/queue/"

    let private getStartPage () =
        async {
            let! response = Web.Core.Http.get "https://kdmid.ru/"
            return response
        }

    let private getCapchaImage () =
        async {
            let! response = Web.Core.Http.get "https://kdmid.ru/captcha/"
            return response
        }

    let private solveCapcha (image: byte[]) =
        async {
            let! response = Web.Core.Http.post "https://kdmid.ru/captcha/" image
            return response
        }

    let private postStartPage (data: string) =
        async {
            //let! response = Web.Core.Http.post "https://kdmid.ru/" data
            return Error "postStartPage not implemented."
        }

    let private getCalendarPage (url: string) =
        async {
            let! response = Web.Core.Http.get url
            return response
        }

    let getKdmidAppointmentsResult (credentials: Credentials) : Async<Result<Set<Appointment>, Kdmid.Error>> =
        async {
            let city, id, cd, ems = credentials.Deconstructed
            let baseUrl = createBaseUrl city
            let credentialParams = createUrlParams id cd ems
            //let! response = getCalendarPage url
            return Error <| Kdmid.Error.InvalidRequest "getKdmidOrderResults not implemented."
        }

    let confirmKdmidOrder city credentials : Async<Result<string, string>> =
        async {
            let baseUrl = createBaseUrl city
            let credentialParams = createUrlParams credentials
            //let! response = getCalendarPage url
            return Error "confirmKdmidOrder not implemented."
        }
