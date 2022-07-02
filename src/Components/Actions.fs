[<RequireQualifiedAccessAttribute>]
module TGG.Components.Actions

open Fable.React
open Fable.React.Props
open TGG.Components

let view state context contextDispatch =
  div [ Class "actions" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Actions" ] ]
    div [ Class "actions-body" ] [
      for t in Action.Type.all ->
        context
        |> Action.Context.get t
        |> ActionContainer.view t state (fun _ -> contextDispatch <| Action.Context.Msg.Toogle t) ] ]
