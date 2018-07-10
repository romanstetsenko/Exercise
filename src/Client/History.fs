module History

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma

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
    | items, AddHistoryItem item -> item::items, Cmd.none   
    | _ -> model, Cmd.none   


let view (model: Model) dispatch =
    let historyItemView (hi: string * HistoryItem) dispatch = 
        let (req, hi') = hi
        Card.card [  ] [
                yield Card.header  [ Common.Props [ OnClick (fun _ -> dispatch (SelectHistoryRequest req) )] ] [ 
                    Card.Header.title [] [
                        str req
                    ]
                    Card.Header.icon [ Common.Props [Title "Send back to the Input text field"] ] [ 
                        i [ ClassName "fa fa-retweet" ] [ ]
                    ]
                ]
                yield Card.content [] [
                    yield match hi' with
                            | HistoryItem.Response res ->
                                match res with 
                                | Ok ok -> 
                                    Message.message [ Message.Color IsSuccess ] [ 
                                        Message.body [ ] [
                                            pre [] [ str ok]
                                        ]
                                    ]
                                | Error err -> 
                                    Message.message [ Message.Color IsWarning ] [ 
                                        Message.body [ ] [
                                            pre  [] [ str err]
                                        ]
                                    ]
                            | HistoryItem.Exn ex -> 
                                Message.message [ Message.Color IsDanger ] [ 
                                    Message.body [ ] [
                                        pre [] [ str ex.Message]
                                    ]
                                ]
                ]
        ]

    Field.div [] [
        yield Label.label [] [ str "History of requests:"]
        for hi in model do
            yield! [historyItemView hi dispatch; br[]]
    ]
    