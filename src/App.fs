module TGG.App

open Fable
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fable.SimpleJson
open Elmish
open Elmish.React
open Elmish.ReactNative
open Browser

#if DEBUG
open Elmish.Debug
#endif

open TGG
open TGG.Components
open TGG.Types

importAll "./App.scss"

let update msg (model: App.Model) =
  match msg with
  | App.ContextMsg msg ->
    match msg with
    | App.Context.SaveContextChange msg -> 
      let (saveModel, cmd) = Save.update msg model.Context.Save
      { model with Context = { model.Context with Save = saveModel } }, Cmd.map App.ContextMsg cmd
    | App.Context.ActionContextChange msg ->
      let (actionModel, cmd) = Action.Context.update msg model.Context.Actions
      { model with Context = { model.Context with Actions = actionModel } }, Cmd.map App.ContextMsg cmd
  | App.StateMsg msg ->
    match msg with
    | App.State.StoreSave ->
      let json = SimpleJson.stringify model.State
      localStorage.setItem("save", json)
      model, Cmd.none
    | App.State.GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (App.StateMsg <| App.State.LoadSave json)
    | App.State.AddItem item ->
      let inventory = model.State.Inventory + item
      { model with State = { model.State with Inventory = inventory } }, Cmd.none
    | App.State.RemoveItem item ->
      let inventory = model.State.Inventory - item
      { model with State = { model.State with Inventory = inventory } }, Cmd.none
    | App.State.ChangeMaxCapBy delta ->
      if delta > 0 then
        { model with State = { model.State with Inventory = { model.State.Inventory with MaxCap = model.State.Inventory.MaxCap+delta } } }, Cmd.none
      else model, Cmd.none
    | App.State.ChangeMaxCapTo maxCap ->
      if maxCap > 0 then
        { model with State = { model.State with Inventory = { model.State.Inventory with MaxCap = maxCap } } }, Cmd.none
      else model, Cmd.none
    | App.State.LoadSave json ->
      if Helpers.notEmpty json then
        let state = Json.parseAs<App.State.Model> json
        { model with State = state; Context = { model.Context with Save = { model.Context.Save with InputValue = Save.Name.get model.State.SaveName } } }, 
          match model.State.SaveName with 
          | Save.Empty -> Cmd.none 
          | _ -> Cmd.ofMsg (App.ContextMsg << App.Context.Msg.SaveContextChange << Save.Context.Msg.AskSaveSet <| false)
      else model, Cmd.batch <| [Cmd.ofMsg << App.StateMsg <| App.State.StoreSave; Cmd.ofMsg << App.ContextMsg << App.Context.SaveContextChange <| Save.Context.AskSaveToogle ]
    | App.State.ChangeSaveName saveName -> 
      (if Helpers.notEmpty saveName then
        { model with State = { model.State with SaveName = Save.Save <| saveName.TrimEnd() } }
      else
        { model with State = { model.State with SaveName = Save.Empty } } )
      , Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.WipeSave ->
      { model with State =  App.State.init() } , Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.Log msg ->
      { model with State = { model.State with Logs = Message(msg)::model.State.Logs } }, Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.ClearLogs ->
      { model with State = { model.State with Logs = [] } }, Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.ActionChange msg ->
      let (state, cmd) = Action.update msg model.State
      { model with State = state }, cmd

let view (model: App.Model) (dispatch: Dispatch<App.Msg>) =
  let dispatchContextChange = dispatch << App.ContextMsg
  let dispatchStateChange = dispatch << App.StateMsg

  fragment [] [
    Save.view model.Context.Save 
              model.State.SaveName 
              (dispatchContextChange << App.Context.SaveContextChange) 
              dispatchStateChange

    div [ Class "container" ] [
      div [ Class "header"  ] [
        h1 [Class "title"] [ str "The Grind Game"]
        h3 [ Class "time" ] [ str << Time.toTextFromSeconds <| model.State.Time ]
        h2 [ Class "save-name" ] [ 
          span 
            [ OnClick (fun _ -> dispatchContextChange << App.Context.SaveContextChange <| Save.Context.AskSaveToogle ) ]
            [ match model.State.SaveName with
              | Save.Save saveName -> 
                str saveName
              | Save.Empty -> 
                str "No Save" ] ] ]
      div [ Class "body"  ] [
        Inventory.view model.State.Inventory
        Actions.view model.State model.Context.Actions dispatch
        div [ Class "automation-logger-container" ] [
          div [ Class "automation" ] [] 
          div [ Class "logger" ] [
            for log in model.State.Logs -> 
              span [ Class "log" ] [ str <| Log.get log ] ] ] ] ] ]

Program.mkProgram App.init update view
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run