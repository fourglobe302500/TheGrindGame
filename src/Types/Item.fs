[<AutoOpen>]
module TGG.Types.Item

type Item = 
  | Empty
  | Pebble
  | Stick

[<RequireQualifiedAccess>]
module Item =
  [<Measure>] type id

  let toString = function
    | Empty -> "Empty"
    | Pebble -> "Pebble"
    | Stick -> "Stick"

  let fromString = function
    | "Pebble" -> Pebble
    | "Stick" -> Stick
    | _ -> Empty

  let fromId = function
    | 1<id> -> Pebble
    | 2<id> -> Stick
    | _ -> Empty

  let getId = function
    | Empty -> 0<id>
    | Pebble -> 1<id>
    | Stick -> 2<id>

  let (|GetItem|) = fromString

  let (|Item|) = toString

  type Amount = ItemAmount of item: Item * count: int

  [<RequireQualifiedAccess>]
  module Amount =
    let get = function ItemAmount (item, count) -> item,count

  type Requirement = ItemRequirement of Amount

  [<RequireQualifiedAccess>]
  module Requirement =
    let get = function ItemRequirement a -> Amount.get a

  type Result = ItemResult of Amount

  [<RequireQualifiedAccess>]
  module Result =
    let get = function ItemResult a -> Amount.get a


type Slot = Slot of Item: Item * Count: int

[<RequireQualifiedAccess>]
module Slot =
  let get = function Slot(item, count) -> item, count