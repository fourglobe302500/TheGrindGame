[<RequireQualifiedAccessAttribute>]
module TGG.Components.Action

open Fable
open Fable.React
open Fable.React.Props
open TGG
open TGG.Components

let types = [
  Gathering
  Combat
  Exploration
  Crafting
  Farming
  Hauling
]

let view (action: Action.State.Model) =
  div [ Class "action-item" ] [
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
              |> List.map (fun (ItemRequirement(ItemAmount(Item.Item item, count))) ->
                span [ Class "requirement-item" ] [ str <| sprintf "%s: %i" item count ] ) ]
        if action.Results.Length <> 0 then
          div [ Class "results" ] [
            yield span [ Class "results-header" ] [ str "Results:" ]
            yield! 
              action.Results
              |> List.map (fun (ItemResult(ItemAmount(Item.Item item, count))) ->
                span [ Class "requirement-item" ] [ str <| sprintf "%s: %i" item count ] ) ] ] ] ]