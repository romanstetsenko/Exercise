module Transformations
open Elmish
open Fable.Helpers.React

type Model = {
    transformations: Transformation.Model list
}

type Msg = 
    | Init 
    | Transform of Transformation.Msg

let init model =
    model, Cmd.none

let update msg model =
    let model' =
        match model, msg with
        | _, Init -> model
    model', Cmd.none   
let transformation = Transformation.webComponent

let view model dispatch =
    div [] [
        for submodel in model.transformations do
            yield transformation.view submodel (Msg.Transform>>dispatch)
    ]

open Structure
let webComponent = {
    init =init
    update = update
    view = view
}