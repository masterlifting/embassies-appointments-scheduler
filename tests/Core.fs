module EmbassyAccess.Core.Tests

open System
open Expecto
open Infrastructure

module Russian =
    open Web.Domain.Http
    open EmbassyAccess.Domain.Core.Internal.Russian

    module private Fixture =
        open Persistence.Storage
        open EmbassyAccess.Domain.Core.Internal

        let request =
            { Id = Guid.NewGuid() |> RequestId
              Value = "https://berlin.kdmid.ru/queue/orderinfo.aspx?id=290383&cd=B714253F"
              Attempt = 1
              Embassy = Russian <| Serbia Belgrade
              Modified = DateTime.UtcNow }

        let requiredHeaders =
            Some
            <| Map [ "Set-Cookie", [ "ASP.NET_SessionId=1"; " AlteonP=1"; " __ddg1_=1" ] ]

        let httpHetStringRequest fileName =
            Environment.CurrentDirectory + "/test_data/" + fileName + ".html"
            |> FileSystem.Context.create
            |> ResultAsync.wrap FileSystem.Read.string
            |> ResultAsync.map (fun data ->
                { Content = data
                  Headers = requiredHeaders
                  StatusCode = 200 })

        let httpPostStringRequest fileName =
            Environment.CurrentDirectory + "/test_data/" + fileName + ".html"
            |> FileSystem.Context.create
            |> ResultAsync.wrap FileSystem.Read.string

        let httpGetBytesRequest fileName =
            Environment.CurrentDirectory + "/test_data/" + fileName
            |> FileSystem.Context.create
            |> ResultAsync.wrap FileSystem.Read.bytes
            |> ResultAsync.map (fun data ->
                { Content = data
                  Headers = requiredHeaders
                  StatusCode = 200 })

        let getAppointmentsDeps =
            { getInitialPage = fun _ _ -> httpHetStringRequest "initial_page_response"
              getCaptcha = fun _ _ -> httpGetBytesRequest "captcha.png"
              solveCaptcha = fun _ -> async { return Ok 42 }
              postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_valid_response"
              postAppointmentsPage = fun _ _ _ -> httpPostStringRequest "appointments_page_has_result_1" }

    open Fixture

    let private ``validation page should have an error`` =
        testAsync "validation page should have an error" {
            let! responseRes =
                request
                |> Russian.API.getAppointments
                    { getAppointmentsDeps with
                        postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_has_error" }

            match Expect.wantError responseRes "Response should have an error" with
            | Operation reason ->
                Expect.equal
                    reason.Code
                    (Some ErrorCodes.PageHasError)
                    $"Error code should be {ErrorCodes.PageHasError}"
            | error -> Expect.isTrue false $"Error should be {nameof Operation} type, but was {error}"
        }

    let private ``validation page should have a confirmation request`` =
        testAsync "validation page should have a confirmation request" {
            let! responseRes =
                request
                |> Russian.API.getAppointments
                    { getAppointmentsDeps with
                        postValidationPage = fun _ _ _ -> httpPostStringRequest "validation_page_requires_confirmation" }

            match Expect.wantError responseRes "Response should have an error" with
            | Operation reason ->
                Expect.equal
                    reason.Code
                    (Some ErrorCodes.NotConfirmed)
                    $"Error code should be {ErrorCodes.NotConfirmed}"
            | error -> Expect.isTrue false $"Error should be {nameof Operation} type, but was {error}"
        }

    let private ``appointments page should not have data`` =
        testTheoryAsync "appointments page should not have data" [ 1; 2; 3; 4; 5; 6; 7 ]
        <| fun i ->
            async {
                let! responseRes =
                    request
                    |> Russian.API.getAppointments
                        { getAppointmentsDeps with
                            postAppointmentsPage =
                                fun _ _ _ -> httpPostStringRequest $"appointments_page_empty_result_{i}" }

                let responseOpt = Expect.wantOk responseRes "Response should be Ok"
                Expect.isNone responseOpt "Response should not be Some"
            }

    let private ``appointments page should have data`` =
        testTheoryAsync "appointments page should have data" [ 1; 2; 3 ]
        <| fun i ->
            async {
                let! responseRes =
                    request
                    |> Russian.API.getAppointments
                        { getAppointmentsDeps with
                            postAppointmentsPage =
                                fun _ _ _ -> httpPostStringRequest $"appointments_page_has_result_{i}" }

                let responseOpt = Expect.wantOk responseRes "Response should be Ok"
                let response = Expect.wantSome responseOpt "Response should be Some"
                Expect.isTrue (not response.Appointments.IsEmpty) "Appointments should not be empty"
            }

    let tests =
        testList
            "Russian"
            [ ``validation page should have a confirmation request``
              ``validation page should have an error``
              ``appointments page should not have data``
              ``appointments page should have data`` ]
