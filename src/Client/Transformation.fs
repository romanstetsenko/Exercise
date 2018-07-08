module Transformation
open Elmish

type Model = Model of string

type Msg = 
    | Init
    | Transform of string


let init model : Model * Cmd<Msg> =
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let model' =
        match model, msg with
        | _, Transform s -> model
        | _, Init -> model
    model', Cmd.none    

let view (Model model) (dispatcher: Msg -> unit) =
    HtmlElements.transformButton model ( fun _ -> dispatcher (Transform model))

open Structure
let webComponent = {
    init = init
    update = update
    view = view
}