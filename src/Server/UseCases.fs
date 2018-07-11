module UseCases
open Model
open Shared
open System
let private logStab _ = ()
let useCase parse sort convert  logger (input: string) =
    let log str l = 
        logger ( sprintf "%s:\n %A" str l)
        l

    input
    |> log "Raw input" 
    |> parse 
    |> log "Parsed result"
    |> Result.map sort 
    |> log "Sorted result"
    |> Result.map convert
    |> log "Converted result"

let bulkUseCase parse sort convert input = 
    let start = DateTime.Now.Ticks
    [1..100000]
    |> List.map (fun _ ->
        async {
           return (useCase parse sort convert logStab input)
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

let handlerWithLoggin logger = handler logger
open Printers
let handleToCsv logger = handlerWithLoggin Csv.print useCase logger
let handleToXml logger = handlerWithLoggin Xml.prettyPrint useCase logger
let handleBulkToCsv = handler Csv.print bulkUseCase

let handleRequest logger = function
    | ConvertToCsv input ->
        handleToCsv logger input
    | ConvertToXml input -> 
        handleToXml logger input
    | ConvertToCsvMultipleTimes input -> 
        handleBulkToCsv input   

