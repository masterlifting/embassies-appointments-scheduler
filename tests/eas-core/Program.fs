module Eas.Program.Tests

open Expecto
open Eas.Core.Tests

let private tests = testList "Tests" [ Russian.tests ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
