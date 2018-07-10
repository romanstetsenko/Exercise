module Presets
open Elmish
open Fable.Helpers.React
open Fulma

type Preset = string * string
type Model = Model of Preset list

type Msg = 
    | SelectPreset of string

let init () =
    let model = 
        Model [
            "Single sentence","""Mary had a little lamb."""
            "Single sentence with abbr","""Mrs. Mary had a little lamb."""
            "Multiple sentences","""Mary had a little lamb. Peter called for the wolf, and Aesop came. Cinderella likes shoes."""
            "Multiple sentences with spaces","""
  Mary   had a little  lamb  . 


  Peter   called for the wolf   ,  and Aesop came .
 Cinderella  likes shoes.
"""
            "Bunch of words","""Mary had a little lamb"""
            "Lorem ipsum", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed sodales aliquet sapien, nec sagittis nisl bibendum at. Cras sagittis nisi ante, eu efficitur metus egestas non. Morbi varius tellus quis nunc ullamcorper, sed hendrerit augue pulvinar. Morbi rutrum urna ac urna imperdiet interdum ac sed nisl. Vivamus sit amet magna faucibus, rutrum leo quis, vehicula ex. Integer eu sodales felis, vel pharetra neque. Sed commodo faucibus tellus, rutrum bibendum massa semper sit amet. Cras interdum ultricies tellus, nec placerat augue vehicula sed. Aliquam dapibus ex nunc, imperdiet interdum nibh convallis ultricies. Donec aliquet aliquet augue, eget rhoncus elit imperdiet vitae. Donec vitae lorem vel lorem lacinia placerat quis id erat. Quisque consequat urna at ex pharetra, id eleifend ligula accumsan. Nunc ut semper quam, vel molestie est. Fusce sed ultrices justo. Proin vitae turpis turpis. Donec lacinia mauris condimentum, suscipit turpis vitae, finibus lacus. Cras eu interdum tellus. Sed rhoncus, risus eu lacinia consectetur, dui quam volutpat dolor, in imperdiet arcu leo eget nibh. Quisque mollis sed orci at imperdiet. Integer sapien orci, tristique sit amet odio vel, rutrum tincidunt quam. Curabitur cursus nisl eros, in pharetra urna cursus dictum. Ut vehicula sit amet ante non efficitur. Cras sed faucibus velit. In ullamcorper mi vel metus malesuada convallis. Ut nisl velit, sagittis nec blandit eu, pellentesque sit amet eros. Nullam lectus felis, fermentum nec mollis finibus, ullamcorper non enim. Fusce venenatis quam sed augue fermentum lacinia. Nullam metus sapien, tempor sit amet auctor ac, accumsan quis tortor. Duis consequat lacus orci. Sed eget mi at turpis aliquet faucibus in eget mi. Nunc a sapien eget mauris viverra volutpat. Praesent rhoncus neque sit amet venenatis laoreet. Pellentesque vehicula consequat ultricies. Ut dolor ligula, vehicula at nisi a, vestibulum malesuada enim. Integer convallis, lectus non gravida iaculis, sem lorem viverra arcu, eu varius arcu mauris ac nunc. Sed volutpat turpis id leo elementum convallis non ut arcu. Maecenas vel sem in elit placerat convallis eget vel erat. Nullam tempor finibus nibh ac viverra. Nullam imperdiet ac ante sit amet iaculis. Etiam ac ultricies risus. Suspendisse ac ex hendrerit, efficitur erat sed, rutrum tellus. Ut sit amet ultricies sapien. Curabitur in enim elementum, dapibus ipsum in, porta leo. Donec ac sem sagittis, mollis urna volutpat, maximus nibh. Duis porttitor purus quis ipsum commodo vestibulum. Pellentesque odio dui, finibus ut libero et, consequat venenatis felis. In felis libero, gravida at interdum non, porttitor scelerisque augue. Cras faucibus euismod felis eget aliquet. Suspendisse cursus lobortis nulla, molestie vehicula turpis fermentum ut. Quisque at nunc id quam malesuada finibus in ut felis. Proin dapibus laoreet massa, et fringilla nisl finibus ut. Phasellus euismod sapien leo, non laoreet ante porta ut. Maecenas pharetra accumsan ullamcorper. Fusce sit amet tortor ipsum. Integer ac ante ligula. Ut hendrerit elit laoreet varius congue. Sed porta nisi malesuada nisi ornare placerat. Nulla facilisi. Mauris tellus diam, condimentum vel massa vitae, gravida maximus ipsum. Aenean sit amet arcu quis magna pretium ultrices quis nec tellus. Mauris at dui nisi. Integer ac luctus nulla. Duis dapibus scelerisque congue. Praesent eget malesuada nulla. Vivamus lobortis metus at iaculis iaculis. Donec blandit tristique leo, quis faucibus nisi laoreet eu. Nullam ultrices tempor convallis."
        ]
    model, Cmd.none
let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model, msg with
    | m, SelectPreset _ -> 
        m, Cmd.none

let toAction dispatcher (description, value) = HtmlElements.actionButton description ( fun _ -> dispatcher (SelectPreset value))

let view (Model model) (dispatcher: Msg -> unit) = 
    Field.div [] [
        Label.label [] [ str "Text samples:"]
        Control.div [] [
            model
            |> List.map (toAction dispatcher)
            |> HtmlElements.actionList
        ]
    ]