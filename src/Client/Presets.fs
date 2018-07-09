module Presets
open Elmish
open Fable.Helpers.React
open Fulma

type Preset = string * string
type Model = Model of Preset list

type Msg = 
    | SelectPreset of string

let init () =
    let model = 
        Model [
            "Single sentence","""Mary had a little lamb."""
            "Multiple sentences","""Mary had a little lamb. Peter called for the wolf, and Aesop came. Cinderella likes shoes."""
        ]
    model, Cmd.none
let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model, msg with
    | m, SelectPreset _ -> 
        m, Cmd.none

let toAction dispatcher (description, value) = HtmlElements.actionButton description ( fun _ -> dispatcher (SelectPreset value))

let view (Model model) (dispatcher: Msg -> unit) = 
    div [] [
        Field.div [] [
            Label.label [] [ str "Text samples:"]
            Control.div [] [
                model
                |> List.map (toAction dispatcher)
                |> HtmlElements.actionList
            ]
        ]
    ]