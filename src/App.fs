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
open Elmish.ReactNative.Components

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

let init () =
  { Context = AppContext.init ()
    State = AppState.init () }
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
    { model with State = stateModel }
    , Cmd.map (function 
      | AppState.ReturnMsg.State msg -> StateMsg msg 
      | AppState.ReturnMsg.Context msg -> ContextMsg msg) cmd

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
            [ OnClick (fun _ -> dispatchContextChange << ContextMsg.SaveContextChange <| SaveContext.AskSaveToogle ) ]
            [ match model.State.SaveName with
              | Save saveName -> 
                str saveName
              | Empty -> 
                str "No Save" ] ] ]
      div [ Class "body"  ] [
        Inventory.view model.State.Inventory
        div [ Class "actions" ] []
        div [ Class "automation-logger-container" ] [
          div [ Class "automation" ] [] 
          div [ Class "logger" ] [
            for log in model.State.Logs -> 
              span [ Class "log" ] [ str <| Log.get log ] ] ] ] ] ]

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run