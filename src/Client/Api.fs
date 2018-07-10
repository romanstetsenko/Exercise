module Api
open Fable.PowerPack.Fetch
open Fable.PowerPack
open System
open Fable.Core.JsInterop

let transformTo requestType text = 
    postRecord "/api/transform" (requestType text) []
    |> Promise.bind( fun r -> r.text() )
    |> Promise.map ofJson<Result<string, string>>
    |> Promise.catch( fun _ex -> 
#if DEBUG 
        Console.WriteLine(_ex)
#endif          
        failwith "Conectivity problems...")
let transformToCsv = 
    transformTo Shared.ConvertToCsv

let transformToXml = 
    transformTo Shared.ConvertToXml

