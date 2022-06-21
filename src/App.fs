module TGG.App

open Fable
open Fable.Core
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop
open Browser
open Fable.Import
open Fable.SimpleJson

#if DEBUG
open Elmish.Debug
#endif

open TGG
open TGG.Types

importAll "./App.scss"

type ContextModel = AppContext.Model
type ContextMsg = AppContext.Msg

type StateModel = AppState.Model
type StateMsg = AppState.Msg

type Model =
  { Context: ContextModel
    State: StateModel}

type Msg =
  | ContextMsg of ContextMsg
  | StateMsg of StateMsg

let contextInit (): ContextModel =
  { Save = Save.init()}

let stateInit (): StateModel = 
  { SaveName = Empty
    Inventory = { Items = []; MaxCap = 10 } }

let init () =
  { Context = contextInit ()
    State = stateInit () }
  , Cmd.ofMsg (StateMsg StateMsg.GetSave)

let update msg (model: Model) =
  match msg with
  | ContextMsg msg ->
    match msg with
    | ContextMsg.SaveContextChange msg -> 
      let (saveModel, cmd) = Save.update msg model.Context.Save
      { model with Context = { model.Context with Save = saveModel } }, Cmd.map ContextMsg cmd
  | StateMsg msg ->
    let (stateModel, cmd) = AppState.update msg model.State
    { model with State = stateModel }, Cmd.map StateMsg cmd

let view model (dispatch: Dispatch<Msg>) =
  let dispatchContextChange = dispatch << ContextMsg
  let dispatchStateChange = dispatch << StateMsg

  fragment [] [
    Save.view model.Context.Save 
              model.State.SaveName 
              (dispatchContextChange << ContextMsg.SaveContextChange) 
              dispatchStateChange

    div [ Class "container" ] [
      div [ Class "header"  ] [
        h1 [Class "title"] [ str "The Grind Game"]
        h2 [ Class "save-name" ] [ 
          span 
            [ OnClick (fun _ -> dispatchContextChange << ContextMsg.SaveContextChange <| SaveContext.AskSaveChange ) ]
            [ match model.State.SaveName with
              | Save saveName -> 
                str saveName
              | Empty -> 
                str "No Save" ] ] ]
      div [ Class "body"  ] [
        Inventory.view model.State.Inventory
        div [ Class "actions" ] []
        div [ Class "automation" ] [] ] ] ]

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
#if DEBUG
// |> Program.withDebugger
#endif
|> Program.run