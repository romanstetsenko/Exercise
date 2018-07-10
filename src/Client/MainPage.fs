module MainPage
open Elmish
open Fable.PowerPack

type Model = {
    presets: Presets.Model
    inputArea: InputArea.Model
    transformations: Transformations.Model
    history: History.Model
    isWaitingServer: bool
}

type TransformTextRes =
    | Response of Result<string, string>
    | Exn of exn

type Msg = 
    | Init
    | Presets of Presets.Msg
    | InputArea of InputArea.Msg
    | Transformations of Transformations.Msg 
    | History of History.Msg
    | TransformTextResult of TransformTextRes //Result<string, string>

let init () : Model * Cmd<Msg> = 
    let ps, psCmd = Presets.init()
    let ia, iaCmd = InputArea.init()
    let ts, tsCmd = Transformations.init()
    let h, hCmd = History.init()
    { 
        presets = ps
        inputArea = ia
        transformations = ts
        history = h
        isWaitingServer = false
    }, Cmd.batch [
        Cmd.map Presets psCmd
        Cmd.map InputArea iaCmd
        Cmd.map Transformations tsCmd
        Cmd.map History hCmd
    ]

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | InputArea msg' -> 
        let res, cmd = InputArea.update msg' model.inputArea
        let tCmd = (if res.hasError then Transformations.Disable else Transformations.Enable) |> Cmd.ofMsg
        { model with inputArea = res }, 
        Cmd.batch [            
            Cmd.map InputArea cmd
            Cmd.map Transformations tCmd
        ]     

    | Init -> failwith "Not Implemented"

    | Presets msg' -> 
        let (Presets.Msg.SelectPreset preset) = msg' 
        let res, cmd = Presets.update msg' model.presets
        { model with presets = res }, 
        Cmd.batch [
            Cmd.map Presets cmd
            Cmd.ofMsg ((InputArea.ChangeContent preset) |> Msg.InputArea)
        ]

    | Transformations msg' ->         
        let res, cmd = Transformations.update msg' model.transformations

        let transformRequest = 
            let textToTransform = model.inputArea.textValue
            match msg' with
            | Transformations.TransformToCsv -> 
                Cmd.ofPromise Api.transformToCsv textToTransform (TransformTextRes.Response>>TransformTextResult) (TransformTextRes.Exn>>TransformTextResult)
            | Transformations.TransformToXml -> 
                Cmd.ofPromise Api.transformToXml textToTransform (TransformTextRes.Response>>TransformTextResult) (TransformTextRes.Exn>>TransformTextResult)
            | _ -> Cmd.none

        { model with transformations = res }, 

        Cmd.batch [ cmd; transformRequest]

    | History(_) -> failwith "Not Implemented"        
    | TransformTextResult (TransformTextRes.Exn ex) -> failwith "Not Implemented"
    | TransformTextResult (TransformTextRes.Response r) -> failwith "Not Implemented"

let view (model: Model) (dispatch: Msg -> unit) =
    let containers = 
        [
            Presets.view model.presets (Msg.Presets>>dispatch)
            InputArea.view model.inputArea (Msg.InputArea>>dispatch)
            Transformations.view model.transformations (Msg.Transformations>>dispatch)
            History.view model.history (Msg.History>>dispatch)
        ] 
        |> List.map (fun el -> HtmlElements.container [] [el]) 

    HtmlElements.section [] containers
    
