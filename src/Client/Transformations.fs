module Transformations
open Elmish
open Fable.Helpers.React
open Fulma

type Model = | Enabled | Disabled

type Msg = 
    | TransformToCsv
    | TransformToXml
    | Enable
    | Disable

let init () =
    Disabled, Cmd.none
let update msg model =
    match msg with
    | Enable ->
        Enabled, Cmd.none
    | Disable ->
        Disabled, Cmd.none
    | _ -> 
        model, Cmd.none    

let actions model dispatch = 
    [
        "CSV", ( fun _ -> dispatch TransformToCsv)
        "XML", ( fun _ -> dispatch TransformToXml)]
    |> List.map (fun (txt, action) -> 
                    match model with 
                    | Enabled -> HtmlElements.actionButton txt action 
                    | Disabled -> HtmlElements.disabledButton txt)
let view model dispatch =
    div [] [
        Field.div [] [
            Label.label [] [ str "Choose output format:"]
            Control.div [] [
                HtmlElements.actionList (actions model dispatch)
            ]
        ]
    ]