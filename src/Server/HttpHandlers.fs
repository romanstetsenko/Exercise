module HttpHandlers
open Giraffe
open UseCases
open Shared
open System.Threading.Tasks
open Microsoft.Extensions.Logging

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
            let defaultLogger = ctx.GetLogger()
            let logger (l: string) = defaultLogger.Log(LogLevel.Information, l) 
            return! Successful.OK (handleRequest logger tr  ) next ctx
        }