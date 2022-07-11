[<RequireQualifiedAccess>]
module TGG.Components.Stats

open Fable.React
open Fable.React.Props
open TGG
open TGG.Types
open Elmish

open Browser

let update msg (model: App.Model) =
  match msg with
  | Stats.ContextMsg msg ->
    match msg with
    | Stats.Context.StatsShowToogle ->
      { model with Context = { model.Context with Stats = { model.Context.Stats with Show = not model.Context.Stats.Show } } }, Cmd.none
  | Stats.StateMsg msg ->
    match msg with
    | Stats.State.RunAction (id, added) ->
      let action = List.find (Action.State.id >> (=) id) model.State.Actions
      let actionsStats = 
        model.State.Stats.Actions
        |> Stats.get
        |> List.tryFind (fst >> (=) id)
        |> function
        | Some _ -> 
          model.State.Stats.Actions
          |> Stats.get
          |> List.map (fun (id, c) -> if id = action.Id then (id, c+1) else (id, c))
          |> Stats.Stats
        | None ->
          model.State.Stats.Actions
          |> Stats.get
          |> fun l -> l@[(id, 1)]
          |> Stats.Stats
      let itemsMade =
        let itemsMade =
          model.State.Stats.ItemsMade |> Stats.get

        itemsMade
        |> List.map fst
        |> List.append (List.map (Slot.get >> fst >> Item.getId) added)
        |> List.distinct
        |> List.sort
        |> List.map (fun id ->
          let item1 =
            itemsMade |> List.tryFind (fst >> (=) id)
          let item2 =
            added |> List.map (Slot.get) |> List.tryFind (fst >> Item.getId >> (=) id)

          match item1, item2 with
          | (Some (_, count1), Some (_, count2)) -> (id, count1+count2)
          | (Some (_, count), None) -> (id, count)
          | (None, Some (_, count)) -> (id, count)
          | (None, None) -> (id, 0) )
        |> Stats.Stats

      let itemsUsed =
        let itemsUsed =
          model.State.Stats.ItemsUsed |> Stats.get
        
        let itemsRequired =
          action.Requirements |> List.map Item.Requirement.get

        itemsUsed
        |> List.map fst
        |> List.append (List.map (fst >> Item.getId) itemsRequired)
        |> List.distinct
        |> List.sort
        |> List.map (fun id ->
          let item1 =
            itemsUsed |> List.tryFind (fst >> (=) id)
          let item2 =
            itemsRequired |> List.tryFind (fst >> Item.getId >> (=) id)
          match item1, item2 with
          | (Some (_, count1), Some (_, count2)) -> (id, count1+count2)
          | (Some (_, count), None) -> (id, count)
          | (None, Some (_, count)) -> (id, count)
          | (None, None) -> (id, 0) )
        |> Stats.Stats

      { 
        model with
          State = {
            model.State with
              Stats = {
                model.State.Stats with
                  Actions = actionsStats
                  ItemsMade = itemsMade
                  ItemsUsed = itemsUsed } } }
      , Cmd.ofMsg << App.StateMsg <| App.State.StoreSave

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

let statsTableView (state: App.State.Model) =
  div [ Class "stats-lists" ] [ 
    statsTableItem 
      (Item.fromId >> Item.toString) 
      "Items Used" 
      state.Stats.ItemsUsed
    statsTableItem 
      (Item.fromId >> Item.toString) 
      "Items Made" 
      state.Stats.ItemsMade
    statsTableItem 
      (fun id -> 
        state.Actions
        |> List.find (Action.State.id >> (=) id)
        |> Action.State.name) 
      "Action" 
      state.Stats.Actions
  ]

let view (context: Stats.Context.Model) state dispatch = 
  Modal.modal
    context.Show [ Id "stats-modal" ]
    [ 
      h1 [] [ str "Statistics" ] 
      button [ OnClick (fun _ -> dispatch Stats.Context.StatsShowToogle) ] [ str "X" ] ]
    [ statsTableView state ]