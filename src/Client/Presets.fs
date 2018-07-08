module Presets
open Elmish
open Fable.Helpers.React

type Model = { 
    presets: Preset.Model list
}
type Msg = 
    | Init
    | PresetMsg of Preset.Msg

let init model =
    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model, msg with
    | _, PresetMsg pm -> 
        match pm with 
        | Preset.Msg.SelectPreset sp ->
            model, Cmd.none
    | _, Init ->
        model, Cmd.none


let view (model: Model) (dispatcher: Msg -> unit) = 
    div [] [
        for presetModel in model.presets do
            yield Preset.webComponent.view presetModel (Msg.PresetMsg >>dispatcher)
    ]

open Structure
let webComponent = {
    init = init
    update = update
    view = view
}