[<RequireQualifiedAccess>]
module TGG.Types.App

open TGG
open TGG.Types
open Elmish

type Stats =
  { Actions: Statistics.Model<int<Action.id>> }

[<RequireQualifiedAccess>]
module Context =
  type Model = 
    { Save: Save.Context.Model
      Actions: Action.Context.Model }

  type Msg =
    | SaveContextChange of Save.Context.Msg
    | ActionContextChange of Action.Context.Msg

  let init () =
    { Save = Save.Context.init ""
      Actions = Action.Context.init() }

[<RequireQualifiedAccess>]
module State =
  type Model = 
    { Inventory: Inventory.State 
      Logs: Log list
      Actions: Action.State.Actions
      SaveName: Save.Name
      Time: int<sec> 
      Stats: Stats}

  let init () = 
    { Time = 0<sec>
      SaveName = Save.Empty
      Actions = Action.initialActions
      Logs = []
      Inventory = { Items = []; MaxCap = 10 } 
      Stats = { Actions = Statistics.Statistics [] }}

  type Msg =
    | StoreSave
    | GetSave
    | AddItem of Item: Item
    | RemoveItem of Item: Item
    | ChangeMaxCapBy of int: int
    | ChangeMaxCapTo of int: int
    | LoadSave of Json: string
    | ChangeSaveName of SaveName: string
    | WipeSave
    | Log of Msg: string
    | ClearLogs
    | ActionChange of Action.State.Msg

type Model =
  { Context: Context.Model
    State: State.Model}

type Msg =
  | ContextMsg of Context.Msg
  | StateMsg of State.Msg

let init () =
  { Context = Context.init ()
    State = State.init () }
  , Cmd.ofMsg (StateMsg State.GetSave)
