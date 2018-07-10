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

open Printers
let handleToCsv = handler Csv.print 
let handleToXml = handler Xml.print

let handleRequest = function
    | ConvertToCsv input ->
        handleToCsv input
    | ConvertToXml input ->
        handleToXml input
