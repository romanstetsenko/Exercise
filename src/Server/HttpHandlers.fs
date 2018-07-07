module HttpHandlers
open Giraffe
open UseCases
open Shared
open System.Threading.Tasks

let private getInitCounter () : Task<Counter> = task { return 42 }

let initHandler = 
    fun next ctx ->
        task {
            let! counter = getInitCounter()
            return! Successful.OK counter next ctx
        }

let transformHandler: HttpHandler =
    fun (next: HttpFunc) ctx ->
        task {
            let! tr = ctx.BindModelAsync<Request>()
            let response = 
                match handleRequest tr with
                | Ok r -> Successful.OK r next ctx 
                | Error r -> RequestErrors.UNPROCESSABLE_ENTITY r next ctx 
            return! response            
        }