module InputArea
open Elmish
open Fable.Helpers.React

type Model = {
    textValue: string
}

type Msg = 
    | Init 
    | SetTextValue of string

let init model =
    model, Cmd.none

let update msg model =
    let model' =
        match model, msg with
        | _, Init -> model
    model', Cmd.none   

let view model dispatch =
    div [] [
        str model.textValue
    ]

open Structure
let webComponent = {
    init =init
    update = update
    view = view
}