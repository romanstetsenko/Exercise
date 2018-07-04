module UseCases
open Model

type Request =
    | ConvertToCsv of string
    | ConvertToXml of string

let useCase parse sort convert input =
    input 
    |> parse 
    |> Result.map sort 
    |> Result.map convert

let handlerConversion converter = 
    useCase (TextAst.parseText TextParsers.parse) TextAst.sortText converter

open Model.Converters
let handleToCsv = handlerConversion Csv.convert 
let handleToXml = handlerConversion Xml.convert

let handleRequest = function
    | ConvertToCsv input ->
        handleToCsv input
    | ConvertToXml input ->
        handleToXml input


let csvHandler = ConvertToCsv >> handleRequest  
let xmlHandler = ConvertToXml >> handleRequest  