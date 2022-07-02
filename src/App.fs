module TGG.App

open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop

#if DEBUG
open Elmish.Debug
#endif

open TGG
open TGG.Components
open TGG.Types

importAll "./App.scss"

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
        Actions.view model.State model.Context.Actions (dispatchContextChange << App.Context.ActionContextChange)
        div [ Class "automation-logger-container" ] [
          div [ Class "automation" ] [] 
          div [ Class "logger" ] [
            for log in model.State.Logs -> 
              span [ Class "log" ] [ str <| Log.get log ] ] ] ] ] ]

Program.mkProgram App.init App.update view
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run