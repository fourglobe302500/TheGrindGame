[<RequireQualifiedAccessAttribute>]
module TGG.Components.Action

open Fable.React
open Fable.React.Props
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
            |> fun inv ->
              console.log inv
              { model with Inventory = inv }, Cmd.ofMsg << App.StateMsg <| App.State.StoreSave

let view (inv: Inventory.State) dispatch (action: Action.State.Model) =
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
                action.Requirements
                |> List.map (fun (Item.ItemRequirement(Item.ItemAmount(Item.Item item, count))) ->
                  span [ Class "requirement-item" ] [ str <| sprintf "%s: %i" item count ] ) ]
          if action.Results.Length <> 0 then
            div [ Class "results" ] [
              yield span [ Class "results-header" ] [ str "Results:" ]
              yield! 
                action.Results
                |> List.map (fun (Item.ItemResult(Item.ItemAmount(Item.Item item, count))) ->
                  span [ Class "requirement-item" ] [ str <| sprintf "%s: %i" item count ] ) ] ] ] ]
    div [ 
      Class <| sprintf "action-item-runner %s" (if Action.canRun inv action then "can-run" else "")
      OnClick <| fun _ -> dispatch << Action.StateMsg << Action.State.Run <| action.Id
    ] [ str "RUN" ] ]