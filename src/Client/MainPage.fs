module MainPage
open Elmish

type Model = {
    presets: Presets.Model
    inputArea: InputArea.Model
    transformations: Transformations.Model
    history: History.Model
    isWaitingServer: bool
}

type Msg = 
    | Init
    | Presets of Presets.Msg
    | InputArea of InputArea.Msg
    | Transformations of Transformations.Msg 
    | History of History.Msg

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
    let handleTransformations msg' model =
        let toApiCommand apiFunc text = 
            Cmd.ofPromise apiFunc text 
                (fun r -> 
                    let r' = History.HistoryItem.Response r
                    History.Msg.AddHistoryItem (text, r'))
                (fun ex ->
                    let ex' = History.HistoryItem.Exn ex
                    History.Msg.AddHistoryItem (text, ex'))

        let res, cmd = Transformations.update msg' model.transformations    
        let transformRequest = 
            let textToTransform = model.inputArea.textValue
            match msg' with
            | Transformations.TransformToCsv -> 
                toApiCommand Api.transformToCsv textToTransform 
            | Transformations.TransformToCsvMultipleTimes -> 
                toApiCommand Api.transformToCsvMultipleTimes textToTransform 
            | Transformations.TransformToXml -> 
                toApiCommand Api.transformToXml textToTransform 
            | _ -> Cmd.none
        { model with transformations = res }, 
            Cmd.batch [ cmd; Cmd.map Msg.History transformRequest ]
    
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
    | Transformations msg' -> handleTransformations msg' model        
    | History msg' -> 
        let res, cmd = History.update msg' model.history
        let subCmd = 
            match msg' with
            | History.Msg.SelectHistoryRequest req -> 
                Cmd.ofMsg ( (InputArea.Msg.ChangeContent req) |> InputArea)
            | _ -> Cmd.none        
        { model with history = res }, Cmd.batch [ cmd; subCmd]

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
    
