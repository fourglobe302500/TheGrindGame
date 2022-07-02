[<RequireQualifiedAccess>]
module Save

open Elmish

type Name =
  | Save of string
  | Empty

[<RequireQualifiedAccess>]
module Name =
  let get = function | Save (name) -> name | Empty -> ""

[<RequireQualifiedAccess>]
module Context =
  type Model = 
    { AskSave: bool
      InputValue: string
      Save: bool
      FileOver: bool }

  type Msg =
    | AskSaveToogle
    | ToogleFileOver
    | InputChange of string
    
  let init () = 
    { AskSave = true
      InputValue = ""
      Save = false
      FileOver = false }

let update (msg) (model: Context.Model) =
  match msg with
  | Context.AskSaveToogle -> { model with AskSave = not model.AskSave }, Cmd.none
  | Context.ToogleFileOver -> { model with FileOver = not model.FileOver }, Cmd.none
  | Context.InputChange s -> { model with InputValue = s }, Cmd.none
