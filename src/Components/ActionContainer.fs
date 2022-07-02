[<RequireQualifiedAccessAttribute>]
module TGG.Components.ActionContainer

open Fable
open Fable.React
open Fable.React.Props
open TGG

let view t model toogle show = 
  div [ Class "action-container" ] [
    div [ Class "action-container-header" ] [
      h2 [ Class "action-container-header-title" ] [ str << ActionType.get <| t ]
      div [ 
        Class "cove action-container-header-show" 
        OnClick (toogle)
      ] [ if show then str "\uf077" else str "\uf078" ] ]
    if show then
      model
      |> List.filter (Action.State.type' >> (=) t)
      |> List.map Action.view
      |> div [ Class "action-items" ] ]