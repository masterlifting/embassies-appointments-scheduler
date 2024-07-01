module Eas.Core.Tests

open Expecto

module Embassies =

    module Russian =
        open System
        open Infrastructure.Dsl
        open Persistence
        open Persistence.Core
        open Persistence.Domain
        open Eas.Persistence
        open Eas.Core
        open Eas.Domain.Internal

        let private ``first page should be parsed`` =
            testAsync "First page should be parsed" {
                let filesPath = Environment.CurrentDirectory + "/test_data"
                let startPage = Domain.FileSystem(filesPath + "/start_page_response.html")

                let result =
                    createStorage startPage
                    |> ResultAsync.wrap (fun storage ->
                        async {
                            match! storage |> Storage.FileSystem.get with
                            | Error e -> return Error e
                            | Ok page ->
                                let request: Request =
                                    { Id = Guid.NewGuid() |> RequestId
                                      User = { Id = UserId 1; Name = "Andrei" }
                                      Embassy = Russian <| Serbia Belgrade
                                      Data =
                                        Map
                                            [ "url",
                                              "https://sarajevo.kdmid.ru/queue/orderinfo.aspx?id=20781&cd=f23cb539&ems=143F4DDF" ]
                                      Modified = DateTime.UtcNow }

                                return!
                                    request
                                    |> Russian.getResponse
                                        { getStartPage = fun _ _ -> async { return Ok page }
                                          postValidationPage = fun _ _ _ -> async { return Ok String.Empty }
                                          getCaptchaImage = fun _ _ -> async { return Ok [||] }
                                          solveCaptchaImage = fun _ -> async { return Ok 1 } }
                        })

                match! result with
                | Ok _ -> return ()
                | Error e -> failwith e

            }

        let tests = testList "Russian.Kdmid" [ ``first page should be parsed`` ]
