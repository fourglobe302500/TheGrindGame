[<RequireQualifiedAccess>]
module TGG.Components.Inventory

open Fable
open Fable.Core
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop
open System

open TGG

let view model =
  div [ Class "inventory" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Inventory" ]
      span [ Class "subtitle" ] [ str <| sprintf "Max Capacity: %i" model.MaxCap ] ]
    div [ Class "inventory-body" ] [
      div [ Class "slots" ] [
        for Slot (item, count) in model.Items ->
          div [ Class "slot" ] [
            div [ Class "item" ] [
              span [ Class "name" ] [ str (Item.toString item) ]
              span [ Class "count" ] [ str <| sprintf "%i/%i" count model.MaxCap ] ] ] ] ] ]

