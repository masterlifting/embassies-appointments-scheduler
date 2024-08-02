module EmbassyAccess.Embassies.Russian.Test

open System
open Expecto
open Infrastructure
open EmbassyAccess
open EmbassyAccess.Embassies.Russian.Domain

module private Fixture =
    open Web.Http.Domain
    open Persistence.FileSystem
    open EmbassyAccess.Domain

    let request =
        { Id = Guid.NewGuid() |> RequestId
          Value = "https://berlin.kdmid.ru/queue/orderinfo.aspx?id=290383&cd=B714253F"
          Attempt = 1
          Embassy = Russian <| Germany Berlin
          Modified = DateTime.UtcNow }

    let requiredHeaders =
        Some
        <| Map [ "Set-Cookie", [ "ASP.NET_SessionId=1"; " AlteonP=1"; " __ddg1_=1" ] ]

    let httpHetStringRequest fileName =
        Environment.CurrentDirectory + $"/embassies/russian/test_data/{fileName}.html"
        |> Storage.create
        |> ResultAsync.wrap Storage.Read.string
        |> ResultAsync.map (fun data ->
            { Content = data
              Headers = requiredHeaders
              StatusCode = 200 })

    let httpPostStringRequest fileName =
        Environment.CurrentDirectory + $"/embassies/russian/test_data/{fileName}.html"
        |> Storage.create
        |> ResultAsync.wrap Storage.Read.string

    let httpGetBytesRequest fileName =
        Environment.CurrentDirectory + "/embassies/russian/test_data/" + fileName
        |> Storage.create
        |> ResultAsync.wrap Storage.Read.bytes
        |> ResultAsync.map (fun data ->
            { Content = data
              Headers = requiredHeaders
              StatusCode = 200 })

    let getAppointmentsDeps =
        { updateRequest = fun _ -> async { return Ok() }
          getInitialPage = fun _ _ -> httpHetStringRequest "initial_page_response"
          getCaptcha = fun _ _ -> httpGetBytesRequest "captcha.png"
          solveCaptcha = fun _ -> async { return Ok 42 }
          postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_valid_response"
          postAppointmentsPage = fun _ _ _ -> httpPostStringRequest "appointments_page_has_result_1" }

open Fixture

let private ``validation page should have an error`` =
    testAsync "validation page should have an error" {
        let deps =
            Api.GetAppointmentsDeps.Russian
                { getAppointmentsDeps with
                    postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_has_error" }

        let! responseRes = request |> Api.getAppointments deps

        match Expect.wantError responseRes "Response should have an error" with
        | Operation reason ->
            Expect.equal reason.Code (Some ErrorCodes.PageHasError) $"Error code should be {ErrorCodes.PageHasError}"
        | error -> Expect.isTrue false $"Error should be {nameof Operation} type, but was {error}"
    }

let private ``validation page should have a confirmation request`` =
    testAsync "validation page should have a confirmation request" {
        let deps =
            Api.GetAppointmentsDeps.Russian
                { getAppointmentsDeps with
                    postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_requires_confirmation" }

        let! responseRes = request |> Api.getAppointments deps

        match Expect.wantError responseRes "Response should have an error" with
        | Operation reason ->
            Expect.equal reason.Code (Some ErrorCodes.NotConfirmed) $"Error code should be {ErrorCodes.NotConfirmed}"
        | error -> Expect.isTrue false $"Error should be {nameof Operation} type, but was {error}"
    }

let private ``appointments page should not have data`` =
    testTheoryAsync "appointments page should not have data" [ 1; 2; 3; 4; 5; 6; 7 ]
    <| fun i ->
        async {
            let deps =
                Api.GetAppointmentsDeps.Russian
                    { getAppointmentsDeps with
                        postAppointmentsPage = fun _ _ _ -> httpPostStringRequest $"appointments_page_empty_result_{i}" }

            let! responseRes = request |> Api.getAppointments deps
            let responseOpt = Expect.wantOk responseRes "Response should be Ok"
            Expect.isNone responseOpt "Response should not be Some"
        }

let private ``appointments page should have data`` =
    testTheoryAsync "appointments page should have data" [ 1; 2; 3 ]
    <| fun i ->
        async {
            let deps =
                Api.GetAppointmentsDeps.Russian
                    { getAppointmentsDeps with
                        postAppointmentsPage = fun _ _ _ -> httpPostStringRequest $"appointments_page_has_result_{i}" }

            let! responseRes = request |> Api.getAppointments deps
            let responseOpt = Expect.wantOk responseRes "Response should be Ok"
            let response = Expect.wantSome responseOpt "Response should be Some"
            Expect.isTrue (not response.Appointments.IsEmpty) "Appointments should not be empty"
        }

let list =
    testList
        "Russian"
        [ ``validation page should have a confirmation request``
          ``validation page should have an error``
          ``appointments page should not have data``
          ``appointments page should have data`` ]
