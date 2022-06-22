[<RequireQualifiedAccess>]
module TGG.Save

open Fable
open Fable.Core
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop
open System
open System.IO
open Fable.Import
open Fable.Import
open Fable.Import
open Browser.Dom
open Browser.Types
open TGG.Types
open Elmish.ReactNative.Components

let [<Global>] console: JS.Console = jsNative

let handleDrop (e: Browser.Types.DragEvent) onload =
  let files = e.dataTransfer.files
  if files.length <> 1 then
    Failure <| sprintf "Invallid number of files: %d" files.length
  else
    let file = files.[0]
    let fr = FileReader.Create()
    fr.addEventListener ("load", fun _ -> onload (string fr.result))
    fr.readAsText file
    Success ()

let init = SaveContext.init

type Msg = SaveContext.Msg
type Model = SaveContext.Model

let update (msg) (model: Model) =
  match msg with
  | Msg.AskSaveToogle -> { model with AskSave = not model.AskSave }, Cmd.none
  | Msg.ToogleFileOver -> { model with FileOver = not model.FileOver }, Cmd.none
  | Msg.InputChange s -> { model with InputValue = s }, Cmd.none

let view (model: Model) saveName dispatchContext dispatchState =
  let dispatchSave _ = 
    if Helpers.notEmpty model.InputValue then
      dispatchState <| AppState.Msg.ChangeSaveName model.InputValue
      dispatchContext <| Msg.AskSaveToogle
  Modal.modal 
    model.AskSave [ Id "save-modal" ]
    [ h1 [] [ str "New Save" ] ] 
    [ 
      div [ Id "save-form" ] [
        label [ HtmlFor "save-input" ] [ str "Enter Save Name:" ]
        input [ 
          Id "save-input"
          AutoFocus true
          OnChange (fun e -> dispatchContext <| Msg.InputChange (e.Value))
          OnKeyPress (fun e -> 
            if e.code = "Enter" then 
              dispatchSave())
          SaveName.get saveName
          |> box 
          |> DefaultValue ]
        button [ 
          Id "save-button"
          OnClick dispatchSave
        ] [ str "Save" ] ]
      div [ Id "save-drop" ] [ 
        div [ 
          Id "inner-save-drop"
          if model.FileOver then 
            Class "file-over"
          OnDrop (fun e -> 
            e.preventDefault()
            e.stopPropagation()
            match handleDrop e 
              (fun content -> 
                dispatchState <| AppState.Msg.LoadSave content
                dispatchContext <| Msg.AskSaveToogle) with
            | Success _ -> ()
            | Failure err -> console.log err )
          OnDragOver (fun e -> 
            e.preventDefault()
            e.stopPropagation() )
          OnDragEnter (fun e -> 
            dispatchContext <| Msg.ToogleFileOver
            e.preventDefault() )
        ] [ str "Drop here" ] ]
      div [ Id "wipe-save" ] [
        span [ 
          Id "wipe-save-button"
          OnClick (fun _ -> 
            dispatchState <| AppState.Msg.WipeSave
            dispatchContext <| Msg.InputChange "")
        ] [ str "Wipe Save" ] ] ]