module History

open Elmish
open Fable.Helpers.React

type Model = {
    historyItems: HistoryItem.Model list
}

type Msg = 
    | Init 
    | HistoryItem of HistoryItem.Msg

let init model =
    model, Cmd.none

let update msg model =
    let model' =
        match model, msg with
        | _, Init -> model
    model', Cmd.none   
let historyItem = HistoryItem.webComponent

let view model dispatch =
    div [] [
        for subModel in model.historyItems do
            yield historyItem.view subModel (Msg.HistoryItem>>dispatch)
    ]

open Structure
let webComponent = {
    init =init
    update = update
    view = view
}