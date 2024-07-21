module Eas.Core.Tests

open Expecto

module Embassies =

    module Russian =
        open System
        open Infrastructure.Domain.Errors
        open Infrastructure.DSL
        open Eas.Domain.Internal.Embassies.Russian

        module private Fixture =
            open Persistence.Storage
            open Eas.Domain.Internal

            let request =
                { Id = Guid.NewGuid() |> RequestId
                  User = { Id = UserId 1; Name = "Andrei" }
                  Embassy = Russian <| Serbia Belgrade
                  Data = Map [ "url", "https://berlin.kdmid.ru/queue/orderinfo.aspx?id=290383&cd=B714253F" ]
                  Modified = DateTime.UtcNow }

            let requiredHeaders =
                Some
                <| Map [ "Set-Cookie", [ "ASP.NET_SessionId=1"; " AlteonP=1"; " __ddg1_=1" ] ]

            let loadHtml fileName =
                Environment.CurrentDirectory + "/test_data/" + fileName
                |> FileSystem.Context.create
                |> ResultAsync.wrap FileSystem.Read.string
                |> ResultAsync.map (fun data -> (data, requiredHeaders))

            let loadImage fileName =
                Environment.CurrentDirectory + "/test_data/" + fileName
                |> FileSystem.Context.create
                |> ResultAsync.wrap FileSystem.Read.bytes
                |> ResultAsync.map (fun data -> (data, requiredHeaders))

            let getResponseDeps =
                { getStartPage = fun _ _ -> loadHtml "start_page_response.html"
                  getCaptchaImage = fun _ _ -> loadImage "captcha_image.png"
                  solveCaptchaImage = fun _ -> async { return Ok 42 }
                  postValidationPage = fun _ _ _ -> loadHtml "validation_page_valid_response.html"
                  postCalendarPage = fun _ _ _ -> loadHtml "calendar_page_has_result_1.html" }

        open Fixture

        let private ``validation page should have an error`` =
            testAsync "validation page should have an error" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseDeps with
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_has_error.html" }

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
                    |> Russian.API.getResponse
                        { getResponseDeps with
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_requires_confirmation.html" }

                match Expect.wantError responseRes "Response should have an error" with
                | Operation reason ->
                    Expect.equal
                        reason.Code
                        (Some ErrorCodes.NotConfirmed)
                        $"Error code should be {ErrorCodes.NotConfirmed}"
                | error -> Expect.isTrue false $"Error should be {nameof Operation} type, but was {error}"
            }

        let private ``calendar page should not have appointments`` =
            testTheoryAsync "calendar page should not have appointments" [ 1; 2; 3; 4; 5; 6; 7 ]
            <| fun i ->
                async {
                    let! responseRes =
                        request
                        |> Russian.API.getResponse
                            { getResponseDeps with
                                postCalendarPage = fun _ _ _ -> loadHtml $"calendar_page_empty_result_{i}.html" }

                    let responseOpt = Expect.wantOk responseRes "Response should be Ok"
                    Expect.isNone responseOpt "Response should not be Some"
                }

        let private ``calendar page should have appointments`` =
            testTheoryAsync "calendar page should have appointments" [ 1; 2; 3 ]
            <| fun i ->
                async {
                    let! responseRes =
                        request
                        |> Russian.API.getResponse
                            { getResponseDeps with
                                postCalendarPage = fun _ _ _ -> loadHtml $"calendar_page_has_result_{i}.html" }

                    let responseOpt = Expect.wantOk responseRes "Response should be Ok"
                    let response = Expect.wantSome responseOpt "Response should be Some"
                    Expect.isTrue (not response.Appointments.IsEmpty) "Appointments should not be empty"
                }

        let tests =
            testList
                "Embassies.Russian"
                [ ``validation page should have a confirmation request``
                  ``validation page should have an error``
                  ``calendar page should not have appointments``
                  ``calendar page should have appointments`` ]
