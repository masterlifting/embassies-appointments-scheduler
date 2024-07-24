module EmbassyAccess.Core.Program

open System
open Expecto
open EmbassyAccess.Core.Tests

let private tests = testList "Tests" [ Russian.tests ]

[<EntryPoint>]
let main args =
    Console.OutputEncoding <- Text.Encoding.UTF8
    runTestsWithCLIArgs [] args tests
