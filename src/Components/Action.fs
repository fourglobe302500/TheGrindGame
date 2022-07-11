[<RequireQualifiedAccess>]
module TGG.Components.Action

open Fable.React
open Fable.React.Props
open TGG
open TGG.Types
open TGG.Components
open Elmish

open Browser

let types = Action.Type.all

let update msg (model: App.State.Model) =
  match msg with
  | Action.State.Run (id) ->
    model.Actions
    |> List.tryFind (Action.State.id >> (=) id)
    |> function
      | None -> model, Cmd.none
      | Some action ->
        if not <| Action.canRun model.Inventory action then model, Cmd.none
        else
          action.Requirements
          |> List.map (Item.Requirement.get)
          |> List.fold (fun inv item -> inv -- item) model.Inventory
          |> fun inv -> 
            action.Results
            |> List.map (Item.Result.get)
            |> List.fold (fun inv item -> inv ++ item) inv
            |> fun finv ->
              let diff = Inventory.dif finv inv
              { 
                model with 
                  Time = Time.addTime model.Time action.Duration; 
                  Inventory = finv; }
              , Cmd.batch [
                Cmd.ofMsg << App.StateMsg <| App.State.StoreSave 
                Cmd.ofMsg << App.StateMsg << App.State.StatsChange << Stats.State.RunAction <| (action.Id, diff) ]

let actionView (inv: Inventory.State) dispatch (action: Action.State.Model) =
  div [ Class "action-item" ] [
    div [ Class "action-item-body" ] [
      h2 [] [ str action.Name ]
      h4 [] [ str << Time.toDuration <| action.Duration ] 
      div [ Class "tooltip"] [ 
        str "see more" 
        div [ Class "tooltip-text action-tooltip" ] [
          if action.Requirements.Length <> 0 then 
            div [ Class "requirements" ] [
              yield span [ Class "requirements-header" ] [ str "Requirements:" ]
              yield!
                action
                |> Action.State.formatRequirements 
                |> List.map (fun e -> span [ Class "requirement-item" ] [e] ) ]
          if action.Results.Length <> 0 then
            div [ Class "results" ] [
              yield span [ Class "results-header" ] [ str "Results:" ]
              yield! 
                action
                |> Action.State.formatResults
                |> List.map (fun e -> span [ Class "result-item" ] [ e ] ) ] ] ] ]
    div [ 
      Class <| sprintf "action-item-runner %s" (if Action.canRun inv action then "can-run" else "")
      OnClick <| fun _ -> dispatch << Action.StateMsg << Action.State.Run <| action.Id
    ] [ str "RUN" ] ]

let actionConteinerView t (model: App.State.Model) dispatch show = 
  div [ Class "action-container" ] [
    div [ Class "action-container-header" ] [
      h2 [ Class "action-container-header-title" ] [ str << Action.Type.get <| t ]
      div [ 
        Class "cove action-container-header-show" 
        OnClick (fun _ -> dispatch << Action.ContextMsg << Action.Context.Toogle <| t)
      ] [ if show then str "\uf077" else str "\uf078" ] ]
    if show then
      model.Actions
      |> List.filter (Action.State.type' >> (=) t)
      |> List.map (actionView model.Inventory dispatch)
      |> div [ Class "action-items" ] ]

let view state context dispatch =
  let dispatch =
    function
    | Action.ContextMsg msg -> dispatch << App.Msg.ContextMsg << App.Context.ActionContextChange <| msg
    | Action.StateMsg msg -> dispatch << App.Msg.StateMsg << App.State.ActionChange <| msg
  div [ Class "actions" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Actions" ] ]
    div [ Class "actions-body" ] [
      for t in Action.Type.all ->
        context
        |> Action.Context.get t
        |> actionConteinerView t state dispatch ] ]