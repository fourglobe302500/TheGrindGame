[<RequireQualifiedAccess>]
module TGG.Inventory

open Fable
open Fable.Core
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop
open System

type Slot = Slot of Item: Item * Count: int

[<RequireQualifiedAccess>]
module Slot =
  let get = function Slot(item, count) -> item, count

type Model = 
  { Inventory: Slot list
    MaxCap: int }

  member t.toJson padding =
    t.Inventory
    |> List.map (fun (Slot (item, count)) ->
      sprintf "{\n\t\t\t%s\"Item\": \"%s\",\n\t\t\t%s\"Count\": \"%i\"\n\t\t%s}" padding (Item.toString item) padding count padding)
    |> String.concat (sprintf ",\n\t\t%s" padding)
    |> fun a -> sprintf "[%s\n\t\t\t%s\t\n\t%s]" padding a padding
    |> fun inv -> sprintf "{\n\t%s\"Inventory\": %s,\n\t%s\"Capacity\": %i\n%s}" padding inv padding t.MaxCap padding

  static member private manipulate f inv item =
    { 
      inv with 
        Inventory = 
          [ for Slot (i, count) in inv.Inventory do
            if i = item then yield Slot(item, f count)
            else yield Slot(i, count) ] }

  static member (+) (inv: Model, item: Item) = 
    Model.manipulate (fun count -> max (count+1) inv.MaxCap) inv item

  static member (-) (inv: Model, item: Item) =
    Model.manipulate (fun count -> min (count-1) 0) inv item

  static member canRemove item m = 
    m.Inventory
    |> List.map (Slot.get >> fst)
    |> List.contains item

type Msg = 
  | AddItem of Item: Item
  | RemoveItem of Item: Item
  | ChangeMaxCap of int: int

let init () = { Inventory = []; MaxCap = 10 }

let update msg model = 
  match msg with
  | AddItem item -> 
    model + item, Cmd.none
  | RemoveItem item -> 
    model - item, Cmd.none
  | ChangeMaxCap maxCap -> 
    { model with MaxCap = maxCap }, Cmd.none

let view model dispatch =
  div [ Class "inventory" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Inventory" ]
      span [ Class "subtitle" ] [ str <| sprintf "Max Capacity: %i" model.MaxCap ]
    ]
    div [ Class "inventory-body" ] [
      div [ Class "slots" ] [
        for Slot (item, count) in model.Inventory do
          div [ Class "slot" ] [
            div [ Class "item" ] [
              span [ Class "name" ] [ str (Item.toString item) ]
              span [ Class "count" ] [ str <| sprintf "%i/%i" count model.MaxCap ]
            ]
          ]
      ]
    ]
  ]

