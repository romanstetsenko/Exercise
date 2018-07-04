module ParseTests
open Expecto
open FParsec


open TextParser
open FsCheck
// let extract = function
//     | Success (s, _ , _) -> s
//     | Failure (f, _ , _) -> f

let testParser p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> errorMsg

module Gens =
    open FsCheck
    type Letter = Letter of char
    type TrivialWord = TrivialWord of string

    type Float01 = Float01 of float
    
    type 'a ListOf100 = ListOf100 of 'a list
    let listOf100Arb() =
        Gen.listOfLength 100 Arb.generate
        |> Arb.fromGen
        |> Arb.convert ListOf100 (fun (ListOf100 l) -> l)
    let float01Arb =
        let maxValue = float 10000
        Arb.convert
            (fun (DoNotSize a) -> float a / maxValue |> Float01)
            (fun (Float01 f) -> f * maxValue + 0.5 |> uint64 |> DoNotSize)
            Arb.from
    //let trivialWordGen = Arb.from

[<Tests>]
let parserTests = 
    testList "Parser tests" [
        testList "pAbbreviation" [
            testCase "Parse own config" <| fun _ ->
                let actual = 
                    config.abbreviations
                    |> List.map (testParser pAbbreviation) 
                Expect.containsAll actual config.abbreviations "default config must be parsed"       
        ]
        testList "pSentenceStop" [
            testCase "Parse own config" <| fun _ ->
                let actual = 
                    config.sentenceStops
                    |> List.map (testParser pSentenceStop) 
                Expect.containsAll actual config.sentenceStops "default config must be parsed"
        ]
        testList "pCompositeWord" [
            testCase "Parse a trivial word" <| fun _ ->
                ()
        ]
    ]