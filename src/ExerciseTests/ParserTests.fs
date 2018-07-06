module ParseTests
open Expecto
open Expecto.Flip
open FParsec


open TextParser
open Model.TextAst

let private testParser p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> errorMsg

let private testWordParser parser input =
    input 
    |> List.map (run parser)
    |> List.filter 
        (fun r ->
            match r with
            | Success _   -> true
            | Failure _ -> false
        ) 
    |> List.map ( fun r -> match r with | Success ((Word w),_,_) -> w)                
    |> Expect.containsAll "Seq is equal" input
let private testSentenceParser parser expected input =
    input 
    |> List.map (run parser)
    |> List.filter 
        (fun r ->
            match r with
            | Success _   -> true
            | Failure _ -> false
        ) 
    |> List.map ( fun r -> match r with | Success (s,_,_) -> s)
    |> Expect.containsAll "Seq is equal" expected


[<Tests>]
let parserTests = 
    testList "Parser tests" [
        testList "pAbbreviation" [
            testCase "Parse own config" <| fun _ ->
                config.abbreviations
                |> List.map (testParser pAbbreviation) 
                |> Expect.containsAll "default config must be parsed" config.abbreviations        
        ]
        testList "pSentenceStop" [
            testCase "Parse own config" <| fun _ ->
                config.sentenceStops
                |> List.map (testParser pSentenceStop) 
                |> Expect.containsAll "default config must be parsed" config.sentenceStops 
        ]
        testList "pSentenceStart and pSentenceWord" [
            testCase "Word starts with a capital letter" <| fun _ ->
                let input = [
                    "A"
                    "AA"
                    "Aa"
                    "AA-1"
                    "Aa-A"
                    "Aa-A1"
                    "Aa-AA"
                    "Aa-Aa"
                    "AAa-1-1"
                    "Aa-1-a"
                    "Aa-A-A"
                    "A1-1-1"
                    "Aa-aa-aaa"
                    "Aa-aA-aAa"
                    "A1-1a-1A"
                ] 
                input |> testWordParser pSentenceStart
                input |> testWordParser pSentenceWord

            testCase "Word starts with a number" <| fun _ ->
                let input = [
                    "1"
                    "11"
                    "1a"
                    "1A-1"
                    "1Aa-A"
                    "1Aa-A1"
                    "1Aa-AA"
                    "1Aa-Aa"
                    "1Aa-1-1"
                    "1Aa-11-11"
                    "1a-A-A"
                    "1A-Aa-AA"
                    "1a-Aa-AA"
                    "1A-1a-a1"
                    "1-11-11"
                    "11-111-1111"
                ] 
                input |> testWordParser pSentenceStart
                input |> testWordParser pSentenceWord

            testCase "Abbreviation can be a word at the start or in the middle of a sentence" <| fun _ ->
                config.abbreviations |> testWordParser pSentenceStart            
                config.abbreviations |> testWordParser pSentenceWord
            
            testCase "other cases not applicable for capilized words" <| fun _ ->
                let input = [
                    "a"
                    "aA"
                    "aA-11"
                    "aA-1a"
                    "aA-1A"
                    "aA-A1"
                    "aA-a1"
                    "aa-a1"
                    "aa-1-11-111"
                ]    
                input |> testWordParser pSentenceWord
        ]
        testList "pSentence" [
            testCase "It parses a sentence" <| fun _ ->
                let input = [
                    "Hi!"
                    "Mr.?"
                    "Mary had a little lamb. Peter called for the wolf, and Aesop came. "
                    "Peter called for the wolf, and Aesop came."
                ]
                let actual = [
                    Sentence [Word "Hi"]
                    Sentence [Word "Mr."]
                    Sentence [Word "Mary"; Word "had"; Word "a"; Word "little"; Word "lamb"]
                    Sentence [Word "Peter"; Word "called"; Word "for"; Word "the"; Word "wolf"; Word "and"; Word "Aesop"; Word "came"]

                ]
                input |> testSentenceParser pSentence actual             
        ]
    ]