module Eas.Core.Program

open Expecto
open Eas.Core.Tests

let private tests = testList "Tests" [ Embassies.Russian.tests ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
