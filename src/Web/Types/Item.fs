[<RequireQualifiedAccess>]
module TGG.Types.Item

open TGG

[<Measure>] type id

type Item = Item of Name: string * Id: int<id>

let get (Item (name, id)) = (name, id)

let getString = get >> fst

let getId = get >> snd

let items = 
  [ Item ("Pebble", 0<id>)
    Item ("Stick", 1<id>) ]

let fromString str items = 
  items
  |> List.find (getString >> (=) str)

let fromId id items=
  items
  |> List.find (getId >> (=) id)

let fromIdToString items id = fromId id items |> getString

let (|GetItem|) items str = fromString str items

type Amount = ItemAmount of itemId: int<id> * count: int

[<RequireQualifiedAccess>]
module Amount =
  let get = function ItemAmount (item, count) -> item,count

type Requirement = ItemRequirement of Amount

[<RequireQualifiedAccess>]
module Requirement =
  let get = function ItemRequirement a -> Amount.get a

[<Measure>] type percent

type Result = ItemResult of Amount: Amount * Chance: float<percent>

[<RequireQualifiedAccess>]
module Result =
  let get = function 
    | ItemResult (a, chance) -> 
      let (item, count) = Amount.get a
      item, count, chance


type Slot = ItemSlot of Item: Item * Count: int

[<RequireQualifiedAccess>]
module Slot =
  let get = function ItemSlot(item, count) -> item, count

  let prettyPrintList label l =
    let map = get >> fun (item, count) -> (item, count) 

    Helpers.prettyItemsLog label l map

