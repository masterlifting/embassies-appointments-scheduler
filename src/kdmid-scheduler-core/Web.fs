module internal KdmidScheduler.Web

module Http =
    open KdmidScheduler.Domain.Core.Kdmid
    open Domain.Core

    let private createUrlParams credentials =
        match credentials with
        | Deconstruct(id, cd, None) -> $"id={id}&cd={cd}"
        | Deconstruct(id, cd, Some ems) -> $"id={id}&cd={cd}&ems={ems}"

    let private createBaseUrl city =
        let cityCode = city |> Mapper.KdmidCredentials.toCityCode
        $"https://{cityCode}.kdmid.ru/queue/"

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

    let getKdmidOrderResults
        (city: Domain.Core.City)
        (credentials: Domain.Core.Kdmid.Credentials)
        : Async<Result<Set<OrderResult> option, string>> =
        async {
            let baseUrl = createBaseUrl city
            let credentialParams = createUrlParams credentials
            //let! response = getCalendarPage url
            return Error "getKdmidCalendar not implemented."
        }

    let confirmKdmidOrder city credentials : Async<Result<string, string>> =
        async {
            let baseUrl = createBaseUrl city
            let credentialParams = createUrlParams credentials
            //let! response = getCalendarPage url
            return Error "confirmKdmidOrder not implemented."
        }
