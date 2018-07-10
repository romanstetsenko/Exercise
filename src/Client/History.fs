module History

open Elmish
open Fable.Helpers.React

type Model = (string*string) list


type Msg = 
    | Reset 
    | SelectHistoryRequest of string
    | Prepend of (string*string)

let init () =
    [], Cmd.none

let update msg model =
    match model, msg with
    | _, Reset -> [], Cmd.none
    | _ -> model, Cmd.none   

let view model dispatch =
    div [] [
        for (request, response) in model do
            yield HtmlElements.historyButton response ( fun _ -> dispatch (SelectHistoryRequest request))
    ]