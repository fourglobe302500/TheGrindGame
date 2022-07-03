[<RequireQualifiedAccess>]
module TGG.Types.Inventory

open TGG.Types
open System

let min = Operators.min

let private rnd = Random()

type State =
  { Items: Slot list
    MaxCap: int }

    static member private manipulate f inv item =
      { 
        inv with 
          Items = 
            if List.exists (Slot.get >> fst >> (=) item) inv.Items then
              inv.Items
              |> List.map (function 
                | Slot(i, count) when i = item -> Slot(i, f count)
                | slot -> slot)
            else
              inv.Items@[Slot(item, f 0) ] }

    static member (+) (inv, item) = 
      State.manipulate (fun count -> min (count+1) inv.MaxCap) inv item

    static member (-) (inv, item) =
      State.manipulate (fun count -> max (count-1) 0) inv item

    static member (++) (inv, (item, counti, chance)) = 
      let n = rnd.NextDouble() * 100.<Item.percent>
      if n <= chance then
        State.manipulate (fun count -> min (count+counti) inv.MaxCap) inv item
      else inv

    static member (--) (inv, (item, counti)) =
      State.manipulate (fun count -> max (count-counti) 0) inv item

let canRemove item m = 
  m.Items
  |> List.map (Slot.get >> fst)
  |> List.contains item