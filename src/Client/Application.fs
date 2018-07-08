module Application
open Elmish
open Fable.Helpers.React

type Model = {
    presets: Presets.Model
    inputArea: InputArea.Model
    transformations: Transformations.Model
    history: History.Model
}

type Msg = 
    | Init
    | PresetsMsg of Presets.Msg
    | InputArea of InputArea.Msg
    | Transformations of Transformations.Msg 
    | History of History.Msg


let init model : Model * Cmd<Msg> =
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let model' =
        match model, msg with
        | _, Init -> model
    model', Cmd.none    

let presets = Presets.webComponent
let inputArea = InputArea.webComponent
let transformations = Transformations.webComponent
let history = History.webComponent


let view (model: Model) (dispatch: Msg -> unit) =
    div [] [
        div [] [
            presets.view model.presets (Msg.PresetsMsg>>dispatch)
        ]
        div [] [
            inputArea.view model.inputArea (Msg.InputArea>>dispatch)
        ]
        div [] [
            transformations.view model.transformations (Msg.Transformations>>dispatch)
        ]
        div [] [
            history.view model.history (Msg.History>>dispatch)
        ]
    ]

open Structure
let webComponent = {
    init = init
    update = update
    view = view
}