[<RequireQualifiedAccess>]
module App

open TGG
open TGG.Types
open Elmish

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
      Time: int<sec> }

  let init () = 
    { Time = 0<sec>
      SaveName = Save.Empty
      Actions = [
        {
          Type = Action.Gathering
          Name = "Get Pebble"
          Requirements = []
          Results = [
            Item.ItemResult <| Item.ItemAmount (Pebble, 1)
          ]
          Duration = Seconds 5<sec>
          Id = 0<Action.id>
        }
        {
          Type = Action.Crafting
          Name = "Hit Peebles"
          Requirements = [
            Item.ItemRequirement <| Item.ItemAmount (Pebble, 2)
            Item.ItemRequirement <| Item.ItemAmount (Stick, 1)
          ]
          Results = [
            Item.ItemResult <| Item.ItemAmount (Pebble, 2)
          ]
          Duration = Seconds 100<sec>
          Id = 1<Action.id>
        }
      ]
      Logs = []
      Inventory = { Items = [
        Slot(Pebble, 1)
        Slot(Stick, 1)
      ]; MaxCap = 10 } }

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
