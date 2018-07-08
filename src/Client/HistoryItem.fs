module HistoryItem
open Elmish

type Model = {
    Request: string
    Response: string
}

type Msg = 
    | Init
    | SelectHistoryRequest of string


let init model : Model * Cmd<Msg> =
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let model' =
        match model, msg with
        | _, SelectHistoryRequest s -> model
        | _, Init -> model
    model', Cmd.none    

let view model (dispatcher: Msg -> unit) =
    HtmlElements.transformButton model.Response ( fun _ -> dispatcher (SelectHistoryRequest model.Request))

open Structure
let webComponent = {
    init = init
    update = update
    view = view
}