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
    | AskSaveChange
    | ToogleFileOver
    | InputChange of string

[<RequireQualifiedAccess>]
module AppContext =
  type Model = 
    { Save: SaveContext.Model }

  type Msg =
    | SaveContextChange of SaveContext.Msg

open Browser
open Fable.SimpleJson
open Fable.Core
open Elmish

[<RequireQualifiedAccess>]
module AppState =
  type Model = 
    { Inventory: InventoryState 
      SaveName: SaveName }

    static member parse json = 
      match SimpleJson.parse json with    
      | Helpers.ParseJsonObj [ "Save"; "Inventory" ] [ JString saveName; inventory ] ->
        match inventory with
        | Helpers.ParseJsonObj [ "Items"; "MaxCap" ] [ items; JNumber maxCap ] ->
          let parseItems = function
            | Helpers.ParseJsonObj [ "Item"; "Count" ] [ JString (Item.GetItem item); JNumber count ] ->
              Some <| Slot(item, int count)
            | _ -> None
          match items with
          | Helpers.ParseJsonArray parseItems items ->
            Some { Inventory = { Items = items; MaxCap = int maxCap }; SaveName = Save saveName }
          | _ -> None
        | _ -> None
      | _ -> None

  type Msg =
    | StoreSave
    | GetSave
    | AddItem of Item: Item
    | RemoveItem of Item: Item
    | ChangeMaxCapBy of int: int
    | ChangeMaxCapTo of int: int
    | LoadSave of Json: string
    | ChangeSaveName of SaveName: string

  let update msg model =
    match msg with
    | StoreSave ->
      let json = SimpleJson.stringify model
      console.log("Saving", json)
      localStorage.setItem("save", json)
      model, Cmd.none
    | GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (LoadSave json)
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
      let loaded = Model.parse json
      console.log("Loaded", loaded)
      match loaded with
      | Some model ->
        model, Cmd.none
      | None ->
        model, Cmd.none
    | ChangeSaveName saveName -> 
      { model with SaveName = Save saveName }, Cmd.none
