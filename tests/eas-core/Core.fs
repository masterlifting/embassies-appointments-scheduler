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
                  Data = Map [ "url", "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20779&cd=99CEBA38" ]
                  Modified = DateTime.UtcNow }

            let getResponseProps =
                { getStartPage = fun _ _ -> async { return Ok(String.Empty, None) }
                  postValidationPage = fun _ _ _ -> async { return Ok String.Empty }
                  getCaptchaImage = fun _ _ -> async { return Ok([||], None) }
                  solveCaptchaImage = fun _ -> async { return Ok 42 } }

            let loadFile fileName =
                Environment.CurrentDirectory + "/test_data/" + fileName
                |> FileSystem.create
                |> ResultAsync.wrap FileSystem.get

            let loadFile' fileName =
                let requiredHeader =
                    Some
                    <| Map [ "Set-Cookie", [ "ASP.NET_SessionId=1"; " AlteonP=1"; " __ddg1_=1" ] ]

                loadFile fileName |> ResultAsync.map (fun data -> (data, requiredHeader))

        open Fixture

        let private ``request should have valid html pipeline`` =
            testAsync "The Validation page should be parsed" {
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
            testList "Embassies.Russian" [ ``request should have valid html pipeline`` ]
