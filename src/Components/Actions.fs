[<RequireQualifiedAccessAttribute>]
module TGG.Components.Actions

open Fable.React
open Fable.React.Props
open TGG.Components
open TGG.Types

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
        |> ActionContainer.view t state dispatch ] ]
