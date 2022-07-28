[<RequireQualifiedAccess>]
module TGG.Core.Types.Stats

type Stat<'t when 't: comparison> = 
    Stat of ('t * int) list

        static member (+) (Stat(l), id) =
            l
            |> List.map (function 
                | (i, c) when i = id -> (i, c+1)
                | s -> s)
            |> Stat

type Model = 
    { Show: bool
      Actions: Stat<int<Action.id>>
      ItemsMade: Stat<int<Item.id>>
      ItemsUsed: Stat<int<Item.id>> }

type Msg =
| RunAction of ActionId: int<Action.id> * AddedItems: Item.Slot list
| StatsShowToogle

let init () =
    { Show = false
      Actions = Stat []
      ItemsMade = Stat []
      ItemsUsed = Stat []}

let get (Stat s) = s