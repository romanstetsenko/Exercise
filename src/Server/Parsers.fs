module Parsers
open Model
open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let hyphen = "-"
let pHyphen = pstring hyphen

let collectParsers list = list |> List.map pstring |> choice

let pAbbriviation =
    ["Mr."; "Mrs."; "P.S."; "D.I.Y"] |> collectParsers


let pSentenceStop =
    [".";"!"; "?"; "…"; "?!"; ] |> collectParsers

let pWordPart = many1Satisfy (fun c -> isLetter c || isDigit c)

let pRegularWord = 
    sepBy1 pWordPart pHyphen
    |>> String.concat hyphen

let pCapitalLetter = satisfy isUpper |>> string 

let pCapitalizedWordTailWithOptHyphen =
    let concatWithHyphen list = String.concat hyphen list
    let tailStartsWithHyphen = pHyphen .>>. (sepBy1 pWordPart pHyphen) |>> (fun (a, b) -> a + concatWithHyphen b)
    let tail = (sepBy pWordPart pHyphen) |>> concatWithHyphen
    tailStartsWithHyphen <|> tail

let pCapitalizedWord = 
    pCapitalLetter .>>. pCapitalizedWordTailWithOptHyphen 
    |>> (fun (a,b) -> a + b)

let pWordSeparator = 
    [","; " - "; ":"; " "] |> collectParsers
    |> many

let pSentenceStart = (pAbbriviation <|> pCapitalizedWord) |>> Word
let pSentenceWord = (pAbbriviation <|> pRegularWord) |>> Word

let pSentence = 
    let concat (head, tail) = head::tail
    (pSentenceStart .>> pWordSeparator)
    .>>. (sepEndBy pSentenceWord pWordSeparator) 
    .>> pSentenceStop 
    |>> concat 
    |>> Sentence

let pText = spaces >>. many1 (spaces >>. pSentence .>> spaces ) .>> spaces |>> InputText
    
let parse t =
    match run pText t with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

