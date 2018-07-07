module TextParser
open Model.TextAst
open FParsec
open Model

let (<?>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

type private UserState = unit
type private Parser<'t> = Parser<'t, UserState>
type Config = {
    abbreviations: string list
    sentenceStops: string list
    wordSeparators: string list
}
let config = {
    abbreviations = ["Mr."; "Mrs."; "P.S."; "D.I.Y"]
    sentenceStops = [".";"!"; "?"; "…";]
    wordSeparators = [","; " - "; ":"; " "]
}
let private hyphen = "-"
let private pHyphen = pstring hyphen <?> "Hyphen"

let private chooseStringParser list = list |> List.map pstring |> choice

let pAbbreviation =
    config.abbreviations |> chooseStringParser <?> "Abbreviation"

let pSentenceStop =
    config.sentenceStops |> chooseStringParser <?> "Sentence stop"

let private pWordPart = many1Satisfy (fun c -> isLetter c || isDigit c)

let private pCompositeWord = 
    sepBy1 pWordPart pHyphen
    |>> String.concat hyphen

let private pCapitalLetter = satisfy (fun c -> isUpper c || isDigit c) |>> string <?> "Capital letter"

let private pCapitalizedWordTailWithOptHyphen =
    let concatWithHyphen list = String.concat hyphen list
    let tailStartsWithHyphen = pHyphen .>>. (sepBy1 pWordPart pHyphen) |>> (fun (a, b) -> a + concatWithHyphen b)
    let tail = (sepBy pWordPart pHyphen) |>> concatWithHyphen
    tailStartsWithHyphen <|> tail

let private pCapitalizedWord = 
    pCapitalLetter .>>. pCapitalizedWordTailWithOptHyphen 
    |>> (fun (a,b) -> a + b) <?> "Capitalized Word"

let pWordSeparator = 
    config.wordSeparators |> chooseStringParser <?> "Word separator"
    |> many 

let private pWord aWord = (pAbbreviation <|> aWord) |>> Word
let pSentenceStart = pCapitalizedWord |> pWord <?> "Sentence start"
let pSentenceWord = pCompositeWord |> pWord <?> "Any word"

let pSentence = 
    let concat (head, tail) = head::tail
    (pSentenceStart .>> pWordSeparator) .>>. (sepEndBy pSentenceWord pWordSeparator) 
    .>> (many1 pSentenceStop) 
    .>> spaces
    |>> concat 
    |>> Sentence 
    <?> "Any sentence"

let pText = spaces >>. many1 (spaces >>. pSentence ) |>> TextAst.Text <?> "Text"

let parse t =
    match run pText t with
    | Success(result, _, _)   -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg

