module Structure
open Fable.Import.React
open Elmish

type WebComponent<'TModel, 'TMsg> = {
    init: 'TModel -> 'TModel * Cmd<'TMsg>
    update: 'TMsg -> 'TModel -> 'TModel * Cmd<'TMsg>
    view: 'TModel -> ('TMsg -> unit) -> ReactElement
}
