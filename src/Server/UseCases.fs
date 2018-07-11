module UseCases
open Model
open Shared
open System

let useCase parse sort convert input =
    input 
    |> parse 
    |> Result.map sort 
    |> Result.map convert

let bulkUseCase parse sort convert input = 
    let start = DateTime.Now.Ticks
    [1..100000]
    |> List.map (fun _ ->
        async {
           return (useCase parse sort convert input)
        }
    )
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.fold (fun (results, errors) result ->
            match result with
            | Ok _ -> (results + 1, errors)
            | Error _ -> (results, errors + 1)) 
        (0,0)
    |> (fun (results, errors) -> 
            let elapsedTime = DateTime.Now.Ticks - start |>( fun ts -> new DateTime(ts) )
            let log = sprintf "%i results. %i errors. Spent %i seconds." results errors (elapsedTime.Second + elapsedTime.Minute * 60)
            if (errors < results) then Result.Ok log else Result.Error log)


let handler converter useCase = 
    useCase TextParser.parse TextAst.sortSentences converter

open Printers
let handleToCsv = handler Csv.print useCase 
let handleToXml = handler Xml.prettyPrint useCase
let handleBulkToCsv = handler Csv.print bulkUseCase

let handleRequest = function
    | ConvertToCsv input ->
        handleToCsv input
    | ConvertToXml input -> 
        handleToXml input
    | ConvertToCsvMultipleTimes input -> 
        handleBulkToCsv input   

