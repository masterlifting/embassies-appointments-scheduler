module Eas.Core.Program

open System
open Expecto
open Eas.Core.Tests

let private tests = testList "Tests" [ Embassies.Russian.tests ]

[<EntryPoint>]
let main args =
    Console.OutputEncoding <- Text.Encoding.UTF8
    runTestsWithCLIArgs [] args tests
