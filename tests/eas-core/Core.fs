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
                  Data = Map [ "url", "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20780&cd=4FC17A57" ]
                  Modified = DateTime.UtcNow }

            let requiredHeaders =
                Some
                <| Map [ "Set-Cookie", [ "ASP.NET_SessionId=1"; " AlteonP=1"; " __ddg1_=1" ] ]

            let getResponseProps =
                { getStartPage = fun _ _ -> async { return Ok(String.Empty, requiredHeaders) }
                  postValidationPage = fun _ _ _ -> async { return Ok(String.Empty, requiredHeaders) }
                  getCaptchaImage = fun _ _ -> async { return Ok([||], requiredHeaders) }
                  solveCaptchaImage = fun _ -> async { return Ok 42 } }

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

        open Fixture

        let private ``validation page should have an error`` =
            testAsync "validation page should have an error" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadHtml "start_page_response.html"
                            getCaptchaImage = fun _ _ -> loadImage "captcha_image.png"
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_invalid_response.html" }

                match Expect.wantError responseRes "Response should have an error" with
                | Operation reason ->
                    Expect.equal reason.Code (Some Errors.ResponseError) $"Error code should be {Errors.ResponseError}"
                | _ -> Expect.isTrue false "Error should be Operation type"
            }

        let private ``validation page should have a confirmation request`` =
            testAsync "validation page should have a confirmation request" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadHtml "start_page_response.html"
                            getCaptchaImage = fun _ _ -> loadImage "captcha_image.png"
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_invalid_requires_confirmation.html" }

                match Expect.wantError responseRes "Response should have an error" with
                | Operation reason ->
                    Expect.equal reason.Code (Some Errors.ResponseError) $"Error code should be {Errors.ResponseError}"
                | _ -> Expect.isTrue false "Error should be Operation type"
            }


        let private ``calendar page should not have appointments`` =
            testAsync "calendar page should not have appointments" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadHtml "start_page_response.html"
                            getCaptchaImage = fun _ _ -> loadImage "captcha_image.png"
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_valid_response.html" }

                let responseOpt = Expect.wantOk responseRes "Response should be Ok"
                Expect.isNone responseOpt "Response should not be Some"
            }

        let private ``calendar page should have appointments`` =
            testAsync "calendar page should have appointments" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadHtml "start_page_response.html"
                            getCaptchaImage = fun _ _ -> loadImage "captcha_image.png"
                            postValidationPage = fun _ _ _ -> loadHtml "validation_page_valid_response.html" }

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
