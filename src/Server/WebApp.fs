module WebApp
open Giraffe 
open Shared

let root : HttpHandler =
    choose [
        route Routes.Api.init >=> HttpHandlers.initHandler
        route Routes.Api.transform >=> POST >=> HttpHandlers.transformHandler 
        ]
