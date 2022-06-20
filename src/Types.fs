[<AutoOpen>]
module TGG.Types

type GlobalMsg =
  | SaveSave
  | GetSave

type Result<'s, 'f> =
  | Success of 's
  | Failure of 'f

type Item = 
  | Pebble
  | Stick
  | Empty

[<RequireQualifiedAccess>]
module Item =
  let toString = function
    | Pebble -> "Pebble"
    | Stick -> "Stick"
    | Empty -> "Empty"

  let fromString = function
    | "Pebble" -> Some Pebble
    | "Stick" -> Some Stick
    | "Empty" -> Some Empty
    | _ -> None

type SaveName =
  | Save of string
  | Empty

[<RequireQualifiedAccess>]
module SaveName =
  let get = function | Save (name) -> name | Empty -> ""
