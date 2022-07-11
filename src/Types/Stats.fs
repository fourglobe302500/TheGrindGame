[<RequireQualifiedAccess>]
module TGG.Types.Stats

type Model<'t when 't: comparison> = 
  Stats of ('t * int) list

    static member (+) (Stats(l), id) =
      l
      |> List.map (function 
        | (i, c) when i = id -> (i, c+1)
        | s -> s)
      |> Stats

[<RequireQualifiedAccess>]
module State =
  type Model =
    { Actions: Model<int<Action.id>>
      ItemsMade: Model<int<Item.id>>
      ItemsUsed: Model<int<Item.id>> }

  type Msg =
    | RunAction of ActionId: int<Action.id> * AddedItems: Slot list

  let init () =
    { Actions = Stats [] 
      ItemsMade = Stats []
      ItemsUsed = Stats [] }

[<RequireQualifiedAccess>]
module Context =
  type Model = 
    { Show: bool }

  type Msg =
    | StatsShowToogle

  let init () = { Show = false }

type Msg =
  | StateMsg of State.Msg
  | ContextMsg of Context.Msg

let get (Stats s) = s