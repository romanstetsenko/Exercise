module UseCases
open Model
open Shared

let useCase parse sort convert input =
    input 
    |> parse 
    |> Result.map sort 
    |> Result.map convert

let handler converter = 
    useCase TextParser.parse TextAst.sortSentences converter

open Model.Converters
let handleToCsv = handler Csv.convert 
let handleToXml = handler Xml.convert

let handleRequest = function
    | ConvertToCsv input ->
        handleToCsv input
    | ConvertToXml input ->
        handleToXml input
