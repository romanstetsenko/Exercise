module HttpHandlers
open Giraffe
open UseCases

[<CLIMutable>]
type ConvertRequest = {
    payload:string
}

let XmlConvertHandler: Giraffe.Core.HttpHandler = 
    fun (next : HttpFunc) ctx ->
        task {
            let! request = ctx.BindJsonAsync<ConvertRequest>()
            let response = 
                match handleRequest (ConvertToXml request.payload) with
                | Ok r -> Successful.OK r next ctx 
                | Error r -> RequestErrors.UNPROCESSABLE_ENTITY r next ctx 
            return! response
        }

let CsvConvertHandler: Giraffe.Core.HttpHandler = 
    fun (next : HttpFunc) ctx ->
        task {
            let! request = ctx.BindJsonAsync<ConvertRequest>()
            let response = 
                match handleRequest (ConvertToCsv request.payload) with
                | Ok r -> Successful.OK r next ctx 
                | Error r -> RequestErrors.UNPROCESSABLE_ENTITY r next ctx 
            return! response
        }
