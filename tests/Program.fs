module EmbassyAccess.Core.Program

open System
open Expecto

let private tests = testList "Tests" [ EmbassyAccess.Embassies.Russian.Tests.list ]

[<EntryPoint>]
let main args =
    Console.OutputEncoding <- Text.Encoding.UTF8
    runTestsWithCLIArgs [] args tests
