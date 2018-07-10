module History

open Elmish
open Fable.Helpers.React

type HistoryItem = 
    | Response of Result<string, string>
    | Exn of exn 

type Model = (string * HistoryItem) list

type Msg = 
    | Reset 
    | SelectHistoryRequest of string
    | AddHistoryItem of (string * HistoryItem)

let init () =
    [], Cmd.none

let update msg model =
    match model, msg with
    | _, Reset -> [], Cmd.none
    | _ -> model, Cmd.none   

let view (model: Model) dispatch =
    div [] [
        for hi in model do
            yield div [] [
                str (sprintf "%A" hi)
            ]//HtmlElements.historyButton response ( fun _ -> dispatch (SelectHistoryRequest request))
    ]