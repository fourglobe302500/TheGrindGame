[<AutoOpen>]
module TGG.Types

type Result<'s, 'f> =
  | Success of 's
  | Failure of 'f

type Item = 
  | Pebble
  | Stick
  | Empty

[<RequireQualifiedAccess>]
module Item =
  let toString = function
    | Pebble -> "Pebble"
    | Stick -> "Stick"
    | Empty -> "Empty"

  let fromString = function
    | "Pebble" -> Some Pebble
    | "Stick" -> Some Stick
    | "Empty" -> Some Empty
    | _ -> None

  let (|GetItem|_|) = fromString

type SaveName =
  | Save of string
  | Empty

[<RequireQualifiedAccess>]
module SaveName =
  let get = function | Save (name) -> name | Empty -> ""

type Slot = Slot of Item: Item * Count: int

[<RequireQualifiedAccess>]
module Slot =
  let get = function Slot(item, count) -> item, count

type InventoryState =
  { Items: Slot list
    MaxCap: int }

    static member private manipulate f inv item =
      { 
        inv with 
          Items = 
            [ for Slot (i, count) in inv.Items do
              if i = item then yield Slot(item, f count)
              else yield Slot(i, count) ] }

    static member (+) (inv, item) = 
      InventoryState.manipulate (fun count -> max (count+1) inv.MaxCap) inv item

    static member (-) (inv, item) =
      InventoryState.manipulate (fun count -> min (count-1) 0) inv item

    static member canRemove item m = 
      m.Items
      |> List.map (Slot.get >> fst)
      |> List.contains item

[<RequireQualifiedAccess>]
module SaveContext =
  type Model = 
    { AskSave: bool
      InputValue: string
      Save: bool
      FileOver: bool }

  type Msg =
    | AskSaveToogle
    | ToogleFileOver
    | InputChange of string
    
  let init () = 
    { AskSave = true
      InputValue = ""
      Save = false
      FileOver = false }

[<RequireQualifiedAccess>]
module AppContext =
  type Model = 
    { Save: SaveContext.Model }

  type Msg =
    | SaveContextChange of SaveContext.Msg

  let init () =
    { Save = SaveContext.init()}

open Browser
open Fable.SimpleJson
open Fable.Core
open Elmish

[<RequireQualifiedAccess>]
module AppState =
  type Model = 
    { Inventory: InventoryState 
      SaveName: SaveName }

  let init () = 
    { SaveName = Empty
      Inventory = { Items = []; MaxCap = 10 } }

  type Msg =
    | StoreSave
    | GetSave
    | AddItem of Item: Item
    | RemoveItem of Item: Item
    | ChangeMaxCapBy of int: int
    | ChangeMaxCapTo of int: int
    | LoadSave of Json: string
    | ChangeSaveName of SaveName: string
    | WipeSave

  type ReturnMsg =
    | Context of AppContext.Msg
    | State of Msg

  let update msg model =
    match msg with
    | StoreSave ->
      let json = SimpleJson.stringify model
      console.log("Saving", json)
      localStorage.setItem("save", json)
      model, Cmd.none
    | GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (State <| LoadSave json)
    | AddItem item ->
      let inventory = model.Inventory + item
      { model with Inventory = inventory }, Cmd.none
    | RemoveItem item ->
      let inventory = model.Inventory - item
      { model with Inventory = inventory }, Cmd.none
    | ChangeMaxCapBy delta ->
      if delta > 0 then
        { model with Inventory = { model.Inventory with MaxCap = model.Inventory.MaxCap+delta } }, Cmd.none
      else model, Cmd.none
    | ChangeMaxCapTo maxCap ->
      if maxCap > 0 then
        { model with Inventory = { model.Inventory with MaxCap = maxCap } }, Cmd.none
      else model, Cmd.none
    | LoadSave json ->
      console.log("Loading", json)
      if Helpers.notEmpty json then
        let model = Json.parseAs<Model> json
        console.log("Loaded", model)
        model, 
          match model.SaveName with 
          | Empty -> Cmd.none 
          | _ -> Cmd.ofMsg (Context << AppContext.Msg.SaveContextChange <| SaveContext.Msg.AskSaveToogle)
      else model, Cmd.ofMsg (State StoreSave)
    | ChangeSaveName saveName -> 
      (if Helpers.notEmpty saveName then
        { model with SaveName = Save <| saveName.TrimEnd() }
      else
        { model with SaveName = Empty })
      , Cmd.ofMsg (State StoreSave)
    | WipeSave ->
      init(), Cmd.ofMsg (State StoreSave)

