[<RequireQualifiedAccess>]
module TGG.Core.Types.Save

open Elmish

type Name =
    | Save of string
    | Empty

[<RequireQualifiedAccess>]
module Name =
    let empty = ""

    let get = function | Save (name) -> name | Empty -> empty

type Model =
    { AskSave: bool
      InputValue: string
      Save: bool
      FileOver: bool
      SaveName: Name }

type Msg =
| AskSaveToogle
| AskSaveSet of bool
| ToogleFileOver
| InputChange of string
| ChangeSaveName of Name

let init () = 
    { AskSave = false
      InputValue = ""
      Save = false
      FileOver = false
      SaveName = Empty }

