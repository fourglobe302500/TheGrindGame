[<RequireQualifiedAccessAttribute>]
module TGG.Components.Actions

open Fable
open Fable.React
open Fable.React.Props
open TGG
open TGG.Components
open TGG.Types

let view model context contextDispatch =
  div [ Class "actions" ] [
    div [ Class "header sub" ] [
      h3 [ Class "title" ] [ str "Actions" ] ]
    div [ Class "actions-body" ] [
      for t in ActionType.all ->
        context
        |> Action.Context.get t
        |> ActionContainer.view t model (fun _ -> contextDispatch <| Action.Context.Msg.Toogle t) ] ]
