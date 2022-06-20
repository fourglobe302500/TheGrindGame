[<RequireQualifiedAccess>]
module TGG.Modal

open Fable
open Fable.Core
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Elmish.ReactNative
open Fable.Core.JsInterop

let modal show props header body =
  if show then 
    div [ yield Class "modal"; yield! props ] [
      div [ Class "modal-content" ] [
        div [ Class "modal-header" ] header 
        div [ Class "modal-body" ]body
      ]
    ]
  else fragment [] []