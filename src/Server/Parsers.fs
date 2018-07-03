module Parsers
open Model.TextAst
open FParsec

type private UserState = unit
type private Parser<'t> = Parser<'t, UserState>

let private hyphen = "-"
let private pHyphen = pstring hyphen

let private chooseStringParser list = list |> List.map pstring |> choice

let pAbbriviation =
    ["Mr."; "Mrs."; "P.S."; "D.I.Y"] |> chooseStringParser

let pSentenceStop =
    [".";"!"; "?"; "…"; "?!"; ] |> chooseStringParser

let private pWordPart = many1Satisfy (fun c -> isLetter c || isDigit c)

let pCompositeWord = 
    sepBy1 pWordPart pHyphen
    |>> String.concat hyphen

let private pCapitalLetter = satisfy isUpper |>> string 

let private pCapitalizedWordTailWithOptHyphen =
    let concatWithHyphen list = String.concat hyphen list
    let tailStartsWithHyphen = pHyphen .>>. (sepBy1 pWordPart pHyphen) |>> (fun (a, b) -> a + concatWithHyphen b)
    let tail = (sepBy pWordPart pHyphen) |>> concatWithHyphen
    tailStartsWithHyphen <|> tail

let pCapitalizedWord = 
    pCapitalLetter .>>. pCapitalizedWordTailWithOptHyphen 
    |>> (fun (a,b) -> a + b)

let pWordSeparator = 
    [","; " - "; ":"; " "] |> chooseStringParser
    |> many

let private pWord aWord = (pAbbriviation <|> aWord) |>> Word
let private pSentenceStart = pCapitalizedWord |> pWord 
let private pSentenceWord = pCompositeWord |> pWord

let pSentence = 
    let concat (head, tail) = head::tail
    (pSentenceStart .>> pWordSeparator)
    .>>. (sepEndBy pSentenceWord pWordSeparator) 
    .>> pSentenceStop 
    |>> concat 
    |>> InputSentence

let pText = spaces >>. many1 (spaces >>. pSentence .>> spaces ) .>> spaces |>> InputText
    
let parse t =
    match run pText t with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

