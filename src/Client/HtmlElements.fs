module HtmlElements
open Fulma
open Fable.Helpers.React


let actionButton txt onClick= 
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let presetButton = actionButton

let transformButton = actionButton