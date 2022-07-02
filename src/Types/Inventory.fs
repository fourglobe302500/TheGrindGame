[<RequireQualifiedAccess>]
module Inventory

open TGG.Types

type State =
  { Items: Slot list
    MaxCap: int }

    static member private manipulate f inv item =
      { 
        inv with 
          Items = 
            [ for Slot (i, count) in inv.Items do
              if i = item then yield Slot(item, f count)
              else yield Slot(i, count) ] }

    static member (+) (inv, item) = 
      State.manipulate (fun count -> max (count+1) inv.MaxCap) inv item

    static member (-) (inv, item) =
      State.manipulate (fun count -> Operators.min (count-1) 0) inv item

let canRemove item m = 
  m.Items
  |> List.map (Slot.get >> fst)
  |> List.contains item