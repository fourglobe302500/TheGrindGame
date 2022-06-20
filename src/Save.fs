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

type Model = 
  { SaveName: SaveName
    AskSave: bool
    InputValue: string
    Save: bool
    FileOver: bool }
  
  member t.toJson () = 
    SaveName.get t.SaveName
    |> sprintf "\"%s\""

type Msg =
  | ChangeSave of SaveName: string
  | AskSaveChange
  | ToogleFileOver
  | InputChange of string
  | LoadSave of string

let init () = 
  { SaveName = SaveName.Empty; 
    AskSave = false
    InputValue = ""
    Save = false
    FileOver = false }

let update msg (model: Model) =
  match msg with
  | ChangeSave saveName -> 
    { model with SaveName = Save saveName; AskSave = false; Save = true; InputValue = ""; FileOver = false }, Cmd.none
  | AskSaveChange -> 
    { model with AskSave = true }, Cmd.none
  | InputChange value -> 
    { model with InputValue = value }, Cmd.none
  | ToogleFileOver -> 
    { model with FileOver = not model.FileOver }, Cmd.none

let view model dispatch =
  Modal.modal 
    model.AskSave [ Id "save-modal" ]
    [ h1 [] [ str "New Save" ] ] 
    [ 
      div [ Id "save-form" ] [
        label [ HtmlFor "save-input" ] [ str "Enter Save Name:" ]
        input [ 
          Id "save-input"
          AutoFocus true
          OnChange (fun e -> dispatch <| InputChange (e.Value))
          OnKeyPress (fun e -> 
            if e.code = "Enter" then dispatch <| ChangeSave model.InputValue)
          SaveName.get model.SaveName
          |> box 
          |> DefaultValue
        ]
        button [ 
          Id "save-button"
          OnClick (fun _ -> dispatch <| ChangeSave model.InputValue)
        ] [ str "Save" ] 
      ]
      div [ 
        Id "save-drop" 
      ] [ 
        div [ 
          Id "inner-save-drop"
          if model.FileOver then 
            Class "file-over"
          OnDrop (fun e -> 
            e.preventDefault()
            e.stopPropagation()
            match handleDrop e (LoadSave >> dispatch) with
            | Success _ -> ()
            | Failure err -> console.log err
          )
          OnDragOver (fun e -> 
            e.preventDefault()
            e.stopPropagation()
          )
          OnDragEnter (fun e -> 
            dispatch <| ToogleFileOver
            e.preventDefault()
          )
        ] [
          str "Drop here" ]
      ] 
    ]