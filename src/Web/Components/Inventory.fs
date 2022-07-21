[<RequireQualifiedAccess>]
module TGG.Components.Inventory

open Fable.React
open Fable.React.Props
open Elmish

open TGG.Types

let update msg (inventory: Inventory.State.Model) =
  match msg with 
  | Inventory.State.AddItem item ->
    let inventory = inventory + item
    inventory, Cmd.ofMsg App.State.TestEvents
  | Inventory.State.RemoveItem item ->
    let inventory = inventory - item
    inventory, Cmd.ofMsg App.State.TestEvents
  | Inventory.State.ChangeMaxCapBy delta ->
    if delta > 0 then
      { inventory with MaxCap = inventory.MaxCap + delta}, Cmd.none
    else inventory, Cmd.none
  | Inventory.State.ChangeMaxCapTo maxCap ->
    if maxCap > 0 then
      { inventory  with MaxCap = maxCap }, Cmd.none
    else inventory, Cmd.none


let view (model: Inventory.State.Model) =
  div [ Class "inventory" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Inventory" ]
      span [ Class "subtitle" ] [ str <| sprintf "Max Capacity: %i" model.MaxCap ] ]
    div [ Class "inventory-body" ] [
      div [ Class "slots" ] [
        for Item.ItemSlot (item, count) in model.Items ->
          div [ Class "slot" ] [
            div [ Class "item" ] [
              span [ Class "name" ] [ str (Item.getString item) ]
              span [ Class "count" ] [ str <| sprintf "%i/%i" count model.MaxCap ] ] ] ] ] ]

