[<RequireQualifiedAccessAttribute>]
module TGG.Components.Action

open Fable.React
open Fable.React.Props
open TGG.Types
open TGG.Components

let types = [
  Action.Gathering
  Action.Combat
  Action.Exploration
  Action.Crafting
  Action.Farming
  Action.Hauling
]

let view (inv: Inventory.State) (action: Action.State.Model) =
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
    div [ Class <| sprintf "action-item-runner %s" (if Action.canRun inv action then "can-run" else "") ] [ str "RUN" ] ]