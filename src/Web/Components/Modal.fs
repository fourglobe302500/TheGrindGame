[<RequireQualifiedAccess>]
module TGG.Web.Components.Modal

open Fable.React
open Fable.React.Props

let modal show props header body =
  if show then 
    div [ yield Class "modal"; yield! props ] [
      div [ Class "modal-content" ] [
        div [ Class "modal-header" ] header 
        div [ Class "modal-body" ]body
      ]
    ]
  else fragment [] []