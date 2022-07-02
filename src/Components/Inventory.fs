[<RequireQualifiedAccess>]
module TGG.Components.Inventory

open Fable.React
open Fable.React.Props

open TGG.Types

let view (model: Inventory.State) =
  div [ Class "inventory" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Inventory" ]
      span [ Class "subtitle" ] [ str <| sprintf "Max Capacity: %i" model.MaxCap ] ]
    div [ Class "inventory-body" ] [
      div [ Class "slots" ] [
        for Item.Slot (item, count) in model.Items ->
          div [ Class "slot" ] [
            div [ Class "item" ] [
              span [ Class "name" ] [ str (Item.toString item) ]
              span [ Class "count" ] [ str <| sprintf "%i/%i" count model.MaxCap ] ] ] ] ] ]

