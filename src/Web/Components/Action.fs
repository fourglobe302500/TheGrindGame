[<RequireQualifiedAccess>]
module TGG.Web.Components.Action

open Fable.React
open Fable.React.Props
open TGG.Core.Types

let types = Action.Type.all

let actionView inv dispatch (action: Action.Action) =
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
                |> Action.Action.formatRequirements Item.items 
                |> List.map (fun e -> span [ Class "requirement-item" ] [ str e ] ) ]
          if action.Results.Length <> 0 then
            div [ Class "results" ] [
              yield span [ Class "results-header" ] [ str "Results:" ]
              yield! 
                action
                |> Action.Action.formatResults Item.items
                |> List.map (fun e -> span [ Class "result-item" ] [ str e ] ) ] ] ] ]
    div [ 
      Class <| sprintf "action-item-runner %s" (if Action.Action.canRun inv action then "can-run" else "")
      OnClick <| fun _ -> dispatch << Action.Run <| action.Id
    ] [ str "RUN" ] ]

let actionConteinerView t (model: App.Model) dispatch show = 
  div [ Class "action-container" ] [
    div [ Class "action-container-header" ] [
      h2 [ Class "action-container-header-title" ] [ str << Action.Type.get <| t ]
      div [ 
        Class "cove action-container-header-show" 
        OnClick (fun _ -> dispatch << Action.ToogleType <| t)
      ] [ if show then str "\uf077" else str "\uf078" ] ]
    if show then
      model.Actions.Actions
      |> List.filter (Action.Action.type' >> (=) t)
      |> List.map (actionView model.Inventory dispatch)
      |> div [ Class "action-items" ] ]

let view (model: App.Model) dispatch =
  let dispatch = dispatch << App.ActionChange
  div [ Class "actions" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Actions" ] ]
    div [ Class "actions-body" ] [
      for t in Action.Type.all ->
        model.Actions
        |> Action.getTypeVis t
        |> actionConteinerView t model dispatch ] ]