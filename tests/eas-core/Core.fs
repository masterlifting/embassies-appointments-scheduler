module Eas.Core.Tests

open Expecto

module Embassies =

    module Russian =
        open System
        open Infrastructure.Domain.Errors
        open Infrastructure.DSL
        open Persistence.Storage

        module private Fixture =
            open Eas.Domain.Internal
            open Eas.Domain.Internal.Embassies.Russian

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
                  postValidationPage = fun _ _ _ -> async { return Ok String.Empty }
                  getCaptchaImage = fun _ _ -> async { return Ok([||], requiredHeaders) }
                  solveCaptchaImage = fun _ -> async { return Ok 42 } }

            let loadFile fileName =
                Environment.CurrentDirectory + "/test_data/" + fileName
                |> FileSystem.create
                |> ResultAsync.wrap FileSystem.get

            let loadFile' fileName =
                loadFile fileName |> ResultAsync.map (fun data -> (data, requiredHeaders))

        open Fixture

        let private ``validation page response should be invalid`` =
            testAsync "Validation page response should be invalid" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadFile' "start_page_response.html"
                            postValidationPage = fun _ _ _ -> loadFile "validation_page_valid_response.html" }

                Expect.equal
                    responseRes
                    (Error <| NotImplemented "searchResponse")
                    "The validation page should be parsed"
            }

        let private ``validation page response should have confirmation request`` =
            testAsync "Validation page response should have confirmation request" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadFile' "start_page_response.html"
                            postValidationPage = fun _ _ _ -> loadFile "validation_page_valid_response.html" }

                Expect.equal
                    responseRes
                    (Error <| NotImplemented "searchResponse")
                    "The validation page should be parsed"
            }

        let private ``request should have valid html pipeline`` =
            testAsync "Request should have valid html pipeline" {
                let! responseRes =
                    request
                    |> Russian.API.getResponse
                        { getResponseProps with
                            getStartPage = fun _ _ -> loadFile' "start_page_response.html"
                            postValidationPage = fun _ _ _ -> loadFile "validation_page_valid_response.html" }

                Expect.equal
                    responseRes
                    (Error <| NotImplemented "searchResponse")
                    "The validation page should be parsed"
            }

        let tests =
            testList
                "Embassies.Russian"
                [ ``validation page response should be invalid``
                  ``validation page response should have confirmation request``
                  ``request should have valid html pipeline`` ]
