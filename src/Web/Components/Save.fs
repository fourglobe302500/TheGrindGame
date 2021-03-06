[<RequireQualifiedAccess>]
module TGG.Components.Save

open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.Import
open Browser.Dom

open TGG
open TGG.Types

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

let view (model: Save.Context.Model) saveName dispatchContext dispatchState =
  let dispatchSave _ = 
    if Helpers.notEmpty model.InputValue then
      dispatchState <| App.State.ChangeSaveName model.InputValue
      dispatchContext <| Save.Context.AskSaveToogle
    elif Save.Name.get saveName <> Save.Name.empty then
      dispatchContext <| Save.Context.AskSaveToogle
  Modal.modal 
    model.AskSave [ Id "save-modal" ]
    [ h1 [] [ str "New Save" ] ] 
    [ 
      div [ Id "save-form" ] [
        label [ HtmlFor "save-input" ] [ str "Enter Save Name:" ]
        input [ 
          Id "save-input"
          AutoFocus true
          OnChange (fun e -> dispatchContext <| Save.Context.InputChange (e.Value))
          OnKeyPress (fun e -> 
            if e.code = "Enter" then 
              dispatchSave())
          Save.Name.get saveName
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
                dispatchState <| App.State.LoadSave content
                dispatchContext <| Save.Context.AskSaveToogle) with
            | Success _ -> ()
            | Failure err -> console.log err )
          OnDragOver (fun e -> 
            e.preventDefault()
            e.stopPropagation() )
          OnDragEnter (fun e -> 
            dispatchContext <| Save.Context.ToogleFileOver
            e.preventDefault() )
        ] [ str "Drop here" ] ]
      div [ Id "wipe-save" ] [
        span [ 
          Id "wipe-save-button"
          OnClick (fun _ -> 
            dispatchContext <| Save.Context.InputChange ""
            dispatchState <| App.State.WipeSave )
        ] [ str "Wipe Save" ] ] ]