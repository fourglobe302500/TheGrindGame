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
    | App.Context.StatsContextChange msg ->
      let (model, cmd) = Stats.update (Stats.ContextMsg msg) model
      model, cmd
  | App.StateMsg msg ->
    match msg with
    | App.State.StoreSave ->
      let json = SimpleJson.stringify model.State
      localStorage.setItem("save", json)
      model, Cmd.none
    | App.State.GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (App.StateMsg <| App.State.LoadSave json)
    | App.State.LoadSave json ->
      if Helpers.notEmpty json then
        let state = Json.parseAs<App.State.Model> json
        { model with State = state; Context = { model.Context with Save = { model.Context.Save with InputValue = Save.Name.get model.State.SaveName } } }, 
          match state.SaveName with 
          | Save.Empty -> Cmd.ofMsg << App.ContextMsg << App.Context.SaveContextChange <| Save.Context.AskSaveToogle
          | _ -> Cmd.ofMsg (App.ContextMsg << App.Context.Msg.SaveContextChange << Save.Context.Msg.AskSaveSet <| false)
      else model, Cmd.batch <| [Cmd.ofMsg << App.StateMsg <| App.State.StoreSave; Cmd.ofMsg << App.ContextMsg << App.Context.SaveContextChange <| Save.Context.AskSaveToogle ]
    | App.State.ChangeSaveName saveName -> 
      (if Helpers.notEmpty saveName then
        { model with State = { model.State with SaveName = Save.Save <| saveName.TrimEnd() } }
      else
        { model with State = { model.State with SaveName = Save.Empty } } )
      , Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.WipeSave ->
      { model with State =  App.State.init() } 
      , Cmd.batch [ 
        Cmd.ofMsg << App.StateMsg <| App.State.StoreSave
        Cmd.ofMsg << App.ContextMsg << App.Context.SaveContextChange << Save.Context.InputChange <| "" ]
    | App.State.Log msg ->
      { model with State = { model.State with Logs = Message(msg)::model.State.Logs } }, Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.ClearLogs ->
      { model with State = { model.State with Logs = [] } }, Cmd.ofMsg (App.StateMsg App.State.StoreSave)
    | App.State.InventoryChange msg ->
      let (inv, cmd) = Inventory.update msg model.State.Inventory
      { model with State = { model.State with Inventory = inv } }, Cmd.map (App.StateMsg) cmd
    | App.State.ActionChange msg ->
      let (state, cmd) = Action.update msg model.State
      { model with State = state }, cmd
    | App.State.StatsChange msg ->
      let (model, cmd) = Stats.update (Stats.StateMsg msg) model
      model, cmd
    | App.State.TestEvents ->
      model.State.Events
      |> List.partition (Events.isMet model.State.Inventory model.State.Stats)
      |> fun (areMet, arentMet) -> 
        let model =
          areMet
          |> List.fold App.Events.update model
        { model with State = { model.State with Events = arentMet } }
        , Cmd.ofMsg << App.StateMsg <| App.State.StoreSave

let view (model: App.Model) (dispatch: Dispatch<App.Msg>) =
  let dispatchContextChange = dispatch << App.ContextMsg
  let dispatchStateChange = dispatch << App.StateMsg

  fragment [] [
    Save.view model.Context.Save 
              model.State.SaveName 
              (dispatchContextChange << App.Context.SaveContextChange) 
              dispatchStateChange
    Stats.view model.Context.Stats
                    model.State
                    (dispatchContextChange << App.Context.StatsContextChange)

    div [ Class "container" ] [
      div [ Class "header"  ] [
        h1 [Class "title"] [ str "The Grind Game"]
        h3 [ Class "time" ] [ str << Time.toTextFromSeconds <| model.State.Time ]
        div [ Class "right" ] [
          h2 [ Class "stats" ] [ 
            span 
              [ OnClick (fun _ -> dispatchContextChange << App.Context.StatsContextChange <| Stats.Context.StatsShowToogle ) ] 
              [ str "Stats" ] ]
          h2 [ Class "save-name" ] [ 
            span 
              [ OnClick (fun _ -> dispatchContextChange << App.Context.SaveContextChange <| Save.Context.AskSaveToogle ) ]
              [ match model.State.SaveName with
                | Save.Save saveName -> 
                  str saveName
                | Save.Empty -> 
                  str "No Save" ] ] ] ]
      div [ Class "body"  ] [
        Inventory.view model.State.Inventory
        Action.view model.State model.Context.Actions dispatch
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