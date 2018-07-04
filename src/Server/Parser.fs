module TextParser
open Model.TextAst
open FParsec
open Model

type private UserState = unit
type private Parser<'t> = Parser<'t, UserState>
type Config = {
    abbreviations: string list
    sentenceStops: string list
}
let config = {
    abbreviations = ["Mr."; "Mrs."; "P.S."; "D.I.Y"]
    sentenceStops = [".";"!"; "?"; "…";]
}
let private hyphen = "-"
let private pHyphen = pstring hyphen

let private chooseStringParser list = list |> List.map pstring |> choice

let pAbbreviation =
    config.abbreviations |> chooseStringParser

let pSentenceStop =
    config.sentenceStops |> chooseStringParser

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

let private pWord aWord = (pAbbreviation <|> aWord) |>> Word
let private pSentenceStart = pCapitalizedWord |> pWord 
let private pSentenceWord = pCompositeWord |> pWord

let pSentence = 
    let concat (head, tail) = head::tail
    (pSentenceStart .>> pWordSeparator)
    .>>. (sepEndBy pSentenceWord pWordSeparator) 
    .>> (many1 pSentenceStop) 
    |>> concat 
    |>> Sentence

let pText = spaces >>. many1 (spaces >>. pSentence .>> spaces ) .>> spaces |>> TextAst.Text
    
let parse t =
    match run pText t with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

