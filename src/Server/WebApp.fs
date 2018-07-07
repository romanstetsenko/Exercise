module WebApp
open Giraffe 

let root : HttpHandler =
    subRoute "/api"
        (
            choose [
                route "/init" >=> HttpHandlers.initHandler
                route  "/transform" >=> POST >=> HttpHandlers.transformHandler 
                ]
        ) 
