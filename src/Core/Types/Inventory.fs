[<RequireQualifiedAccess>]
module TGG.Core.Types.Inventory

open TGG.Core.Types
open System

let private min = Operators.min

let private rnd = Random()

type Model =
    { Items: Item.Slot list
      MaxCap: int }

    static member private manipulate f inv item =
        { inv with 
            Items = 
            if List.exists (Item.Slot.get >> fst >> (=) item) inv.Items then
                inv.Items
                |> List.map (function 
                | Item.ItemSlot(i, count) when i = item -> Item.ItemSlot(i, f count)
                | slot -> slot)
            else
                inv.Items@[Item.ItemSlot(item, f 0) ] }

    static member (+) (inv, item) = 
        Model.manipulate (fun count -> min (count+1) inv.MaxCap) inv item

    static member (-) (inv, item) =
        Model.manipulate (fun count -> max (count-1) 0) inv item

    static member (++) (inv, (item, counti, chance)) = 
        let n = rnd.NextDouble() * 100.<Item.percent>
        if n <= chance then Model.manipulate (fun count -> min (count+counti) inv.MaxCap) inv item
        else inv

    static member (--) (inv, (item, counti)) =
        Model.manipulate (fun count -> max (count-counti) 0) inv item

type Msg =
| AddItem of Item: Item.Item
| RemoveItem of Item: Item.Item
| ChangeMaxCapBy of int: int
| ChangeMaxCapTo of int: int

let init () =
    { Items = []
      MaxCap = 10 }

let items (inv: Model) = inv.Items

let canRemove item (m: Model) = 
    m.Items
    |> List.map (Item.Slot.get >> fst)
    |> List.contains item

let dif (inv1: Model) (inv2: Model) =
    [ for Item.ItemSlot(item, amount) in inv1.Items -> 
        inv2.Items
        |> List.map Item.Slot.get
        |> List.tryFind (fst >> (=) item)
        |> (function
        | Some (_, c) -> (item, max (amount-c) 0)
        | None -> (item, amount) )
        |> Item.ItemSlot ]