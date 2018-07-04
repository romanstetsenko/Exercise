module ParseTests
open Expecto
open FParsec


let testParser p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<Tests>]
let parserTests = 
    testList "Parser tests" [
        testList "Abbreviation tests" [
            testCase "Own config succesfully parsed" <| fun _ ->
                
                
                Expect.isTrue true "An abbr"  
            test "one test" {
                Expect.equal (2+2) 4 "2+2"
            }
        ]
    ]