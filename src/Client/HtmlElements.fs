module HtmlElements
open Fulma
open Fable.Helpers.React

let actionList actions =
    Button.list [] actions
let actionButton txt onClick= 
    Button.span
        [ Button.OnClick onClick ]
        [ str txt ]
let disabledButton txt = 
    Button.span
        [ Button.Option.Disabled true ]
        [ str txt ]
        
let container = Container.container
let section = Section.section
let transformButton = actionButton
let historyButton = actionButton
