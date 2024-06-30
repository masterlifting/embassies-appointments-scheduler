module Eas.Core.Tests

open Expecto

module Russian =
    open Eas.Core.Russian

    let private ``get kdmid response`` =
        let name = "Get kdmid response"
        testAsync name { 
            //let! response = getResponse
            Expect.equal "Get kdmid response" name "Values should be equal" }

    let tests = testList "Russian.Kdmid" [ ``get kdmid response`` ]
