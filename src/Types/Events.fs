[<RequireQualifiedAccess>]
module TGG.Types.Event

open TGG.Types

type Condition =
  | ItemCount of Item.Amount
  | ActionCount of ActionId: int<Action.id> * Count: int

type Model =
  { Condition: Condition list
    Msg: string }

let events = []