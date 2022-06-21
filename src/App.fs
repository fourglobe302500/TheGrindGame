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

importAll "./App.scss"

type Model = 
  { Inventory: Inventory.Model
    Save: Save.Model }
  
  member t.toJson () =
    sprintf "{\n\t\"Inventory\": %s,\n\t\"Save\": %s\n}" (t.Inventory.toJson "\t") (t.Save.toJson ())

  static member serialize =
    (fun json -> 
      console.log json
      json) >> SimpleJson.tryParse >> function
      | None -> None
      | Some json ->
        match json with
        | Helpers.ParseJsonObj [ "save"; "Inventory" ] [ JString save; inventory ] ->
          match inventory with
          | Helpers.ParseJsonObj [ "inventory"; "cap" ] [ inventory; JNumber cap ] ->
            let parseItem = function
              | Helpers.ParseJsonObj [ "item"; "count" ] [ JString item; JNumber count ] ->
                match Item.fromString item with
                  | Some item ->
                    Some <| Inventory.Slot (item, int count)
                  | None -> None
              | _ -> None
            match inventory with
            | Helpers.ParseJsonArray parseItem inv ->
              Some { Inventory = { Inventory = inv; MaxCap = int cap }; Save = { Save.init() with SaveName = Save save } }
            | _ -> None
          | _ -> None
        | _ -> None 

type Msg =
  | InventoryMsg of Msg: Inventory.Msg
  | SaveMsg of Msg: Save.Msg
  | GlobalMsg of Msg: GlobalMsg

let init () = 
  { Save = Save.init ()
    Inventory = Inventory.init() }
  , Cmd.ofMsg (GlobalMsg GetSave)

let update msg (model: Model) =
  match msg with
  | InventoryMsg msg -> 
    let newInventory, cmd = Inventory.update msg model.Inventory
    { model with Inventory = newInventory }, Cmd.map (InventoryMsg) cmd
  | SaveMsg msg ->
    let newSave, cmd = Save.update msg model.Save
    if newSave.Save then 
      { model with Save = { newSave with Save = false } }, Cmd.ofMsg <| GlobalMsg SaveSave
    else 
      { model with Save = newSave }, Cmd.map (SaveMsg) cmd
  | GlobalMsg msg ->
    match msg with
    | SaveSave ->
      localStorage.setItem ("save", model.toJson())
      model, Cmd.none
    | GetSave -> 
      console.log "serializing"
      Model.serialize <| localStorage.getItem ("save")
      |> function
        | Some model ->
          model, Cmd.none
        | None ->
          model, Cmd.none

let view model (dispatch: Dispatch<Msg>) =
  fragment [] [
    Save.view model.Save (SaveMsg >> dispatch)

    div [ Class "container" ] [
      div [ Class "header"  ] [
        h1 [Class "title"] [ str "The Grind Game"]
        h2 [ Class "save-name" ] [ 
          span 
            [ OnClick (fun _ -> dispatch <| SaveMsg Save.Msg.AskSaveChange ) ]
            [ match model.Save.SaveName with
              | Save saveName -> 
                str saveName
              | Empty -> 
                str "No Save" ]
        ]
      ]
      div [ Class "body"  ] [
        Inventory.view model.Inventory (InventoryMsg >> dispatch)
        div [ Class "actions" ] []
        div [ Class "automation" ] []
      ]
    ]
  ]

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
#if DEBUG
// |> Program.withDebugger
#endif
|> Program.run