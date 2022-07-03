[<RequireQualifiedAccessAttribute>]
module TGG.Components.ActionContainer

open Fable.React
open Fable.React.Props
open TGG.Types

let view t (model: App.State.Model) dispatch show = 
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
      |> List.map (Action.view model.Inventory dispatch)
      |> div [ Class "action-items" ] ]