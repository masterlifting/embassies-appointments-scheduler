module Eas.Core.Tests

open Expecto

module Embassies =

    module Russian =
        open System
        open Infrastructure.Domain.Errors
        open Infrastructure.Dsl
        open Persistence

        module private Fixture =
            open Eas.Domain.Internal
            open Eas.Domain.Internal.Embassies.Russian

            let request =
                { Id = Guid.NewGuid() |> RequestId
                  User = { Id = UserId 1; Name = "Andrei" }
                  Embassy = Russian <| Serbia Belgrade
                  Data =
                    Map [ "url", "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                  Modified = DateTime.UtcNow }

            let getResponseProps =
                { getStartPage = fun _ _ -> async { return Ok String.Empty }
                  postValidationPage = fun _ _ _ -> async { return Ok String.Empty }
                  getCaptchaImage = fun _ _ -> async { return Ok [||] }
                  solveCaptchaImage = fun _ -> async { return Ok 1 } }

        open Fixture

        let private ``the first page should be parsed`` =
            testAsync "The First page should be parsed" {
                let! result =
                    Environment.CurrentDirectory + "/test_data/start_page_response.html"
                    |> Storage.FileSystem.create
                    |> ResultAsync.wrap Storage.FileSystem.get
                    |> ResultAsync.bind' (fun page ->
                        request
                        |> Russian.getResponse
                            { getResponseProps with
                                getStartPage = fun _ _ -> async { return Ok page } })

                Expect.equal
                    result
                    (Error <| Business "No nodes found on the validation page.")
                    "The first page should be parsed"
            }

        let private ``the validation page should be parsed`` =
            testAsync "The Validation page should be parsed" {
                let! result =
                    Environment.CurrentDirectory + "/test_data/validation_page_valid_response.html"
                    |> Storage.FileSystem.create
                    |> ResultAsync.wrap Storage.FileSystem.get
                    |> ResultAsync.bind' (fun page ->
                        request
                        |> Russian.getResponse
                            { getResponseProps with
                                postValidationPage = fun _ _ _ -> async { return Ok page } })

                Expect.notEqual
                    result
                    (Error <| Business "No nodes found on the validation page.")
                    "The validation page should be parsed"
            }

        let tests =
            testList
                "Embassies.Russian"
                [ ``the first page should be parsed``
                  ``the validation page should be parsed`` ]
