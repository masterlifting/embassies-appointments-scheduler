module Eas.Core.Tests
open Expecto

module Russian =
    
    let private getResponse =
        let name = "Test"
        testTask name {
            Expect.equal "Test" name "Values should be equal"
        }

    let tests =
        testList "Russian" [
            getResponse
        ]