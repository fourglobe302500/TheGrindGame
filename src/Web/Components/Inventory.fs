[<RequireQualifiedAccess>]
module TGG.Web.Components.Inventory

open Fable.React
open Fable.React.Props

open TGG.Core.Types

let view (model: Inventory.Model) =
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

