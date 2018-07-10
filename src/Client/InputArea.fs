module InputArea
open Elmish
open Fable.Helpers.React
open System
open Fulma

type Model = {
    textValue: string
    contentError: string
    hasError: bool
}
let requiredField (value : string) =
    if String.IsNullOrWhiteSpace value then
        "This field is required"
    else
        ""

type Msg = 
    | ChangeContent of string

let init () =
    {
        textValue = ""
        contentError = "This field is required"
        hasError = true 
    }, Cmd.none

let update msg model =
    match model, msg with
    | m, ChangeContent v ->
        let err = requiredField v 
        {m with 
            textValue = v
            contentError = err
            hasError = String.IsNullOrWhiteSpace err |> not
        }, Cmd.none


open Fable.Core.JsInterop
let view model dispatch =
    div [] [
        Field.div [] [
            Label.label [] [ str "Input text:"]
            Control.div [] [
                Textarea.textarea [
                    Textarea.Color ( if model.hasError then Color.IsDanger else Color.NoColor ) 
                    Textarea.ValueOrDefault model.textValue
                    Textarea.OnChange (fun ev -> !!ev.target?value |> ChangeContent |> dispatch )
                ] []
            ]
            Help.help [ Help.Color IsDanger ] [ str model.contentError ]
        ]
    ]