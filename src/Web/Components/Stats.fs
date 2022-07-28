[<RequireQualifiedAccess>]
module TGG.Web.Components.Stats

open Fable.React
open Fable.React.Props
open TGG.Web
open TGG.Core
open TGG.Core.Types

let statsItemView getLabel item =
  let (id, count) = item
  fragment [] [
    td [] [ str << getLabel <| id ]
    td [] [ str <| sprintf "%i" count ]
  ]

let statsTableItem getLabel label items =
  div [ Class "stats-list" ] [
    table [] [
      thead [] [
        tr [] [ 
          th [] [ str label ]
          th [] [ str "Count" ] ] ]
      items
      |> Stats.get
      |> List.map (statsItemView getLabel >> Helpers.inList >> tr [])
      |> tbody [] ] ]

let statsTableView (state: App.Model) =
  div [ Class "stats-lists" ] [ 
    statsTableItem 
      (fun id -> Item.fromId id Item.items |> Item.getString) 
      "Items Used" 
      state.Stats.ItemsUsed
    statsTableItem 
      (fun id -> Item.fromId id Item.items |> Item.getString) 
      "Items Made" 
      state.Stats.ItemsMade
    statsTableItem 
      (fun id -> 
        state.Actions.Actions
        |> List.find (Action.Action.id >> (=) id)
        |> Action.Action.name) 
      "Action" 
      state.Stats.Actions
  ]

let view (state: App.Model) dispatch = 
  Modal.modal
    state.Stats.Show [ Id "stats-modal" ]
    [ 
      h1 [] [ str "Statistics" ] 
      button [ OnClick (fun _ -> dispatch Stats.StatsShowToogle) ] [ str "X" ] ]
    [ statsTableView state ]