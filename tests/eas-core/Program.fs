module Eas.Program.Tests

open Expecto
open Eas.Core.Tests

[<Tests>]
let tests =
    testList "all"
    [ Russian.tests ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
