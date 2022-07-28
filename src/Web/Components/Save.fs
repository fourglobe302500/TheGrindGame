[<RequireQualifiedAccess>]
module TGG.Web.Components.Save

open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.Import
open Browser.Dom

open TGG.Web
open TGG.Core.Types

let handleDrop (e: Browser.Types.DragEvent) onload =
    let files = e.dataTransfer.files
    if files.length <> 1 then
      Global.Failure <| sprintf "Invallid number of files: %d" files.length
    else
      let file = files.[0]
      let fr = FileReader.Create()
      fr.addEventListener ("load", fun _ -> onload (string fr.result))
      fr.readAsText file
      Global.Success ()

let view (model: Save.Model) dispatch =
    let dispatchSaveMsg = dispatch << App.SaveChange
    let dispatchSave _ = 
        if Helpers.notEmpty model.InputValue then
            dispatchSaveMsg << Save.ChangeSaveName << Save.Save <| model.InputValue
            dispatchSaveMsg <| Save.AskSaveToogle
        elif Save.Name.get model.SaveName <> Save.Name.empty then
            dispatchSaveMsg <| Save.AskSaveToogle
    Modal.modal 
        model.AskSave [ Id "save-modal" ]
        [ h1 [] [ str "New Save" ] ] 
        [ 
        div [ Id "save-form" ] [
            label [ HtmlFor "save-input" ] [ str "Enter Save Name:" ]
            input [ 
            Id "save-input"
            AutoFocus true
            OnChange (fun e -> dispatchSaveMsg <| Save.InputChange (e.Value))
            OnKeyPress (fun e -> 
                if e.code = "Enter" then 
                    dispatchSave())
            Save.Name.get model.SaveName
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
                        dispatch << App.Local << App.LoadSave <| content
                        dispatchSaveMsg <| Save.AskSaveToogle) with
                | Global.Success _ -> ()
                | Global.Failure err -> console.log err )
            OnDragOver (fun e -> 
                e.preventDefault()
                e.stopPropagation() )
            OnDragEnter (fun e -> 
                dispatchSaveMsg <| Save.ToogleFileOver
                e.preventDefault() )
            ] [ str "Drop here" ] ]
        div [ Id "wipe-save" ] [
            span [ 
            Id "wipe-save-button"
            OnClick (fun _ -> 
                dispatchSaveMsg <| Save.InputChange ""
                dispatch <| App.WipeSave )
            ] [ str "Wipe Save" ] ] ]