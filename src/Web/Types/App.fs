[<RequireQualifiedAccess>]
module TGG.Types.App

open TGG
open TGG.Types
open Elmish

[<RequireQualifiedAccess>]
module Context =
  type Model = 
    { Save: Save.Context.Model
      Actions: Action.Context.Model
      Stats: Stats.Context.Model }

  type Msg =
    | SaveContextChange of Save.Context.Msg
    | ActionContextChange of Action.Context.Msg
    | StatsContextChange of Stats.Context.Msg

  let init () =
    { Save = Save.Context.init ()
      Actions = Action.Context.init ()
      Stats = Stats.Context.init () }

[<RequireQualifiedAccess>]
module State =
  type Model = 
    { Inventory: Inventory.State.Model
      Logs: Log list
      Actions: Action.State.Actions
      SaveName: Save.Name
      Time: int<sec> 
      Stats: Stats.State.Model
      Events: Events.Model list }

  let init () = 
    { Time = 0<sec>
      SaveName = Save.Empty
      Actions = Action.initialActions
      Logs = []
      Inventory = { Items = []; MaxCap = 10 } 
      Stats = Stats.State.init () 
      Events = Events.events }

  type Msg =
    | StoreSave
    | GetSave
    | LoadSave of Json: string
    | ChangeSaveName of SaveName: string
    | WipeSave
    | Log of Msg: string
    | ClearLogs
    | InventoryChange of Inventory.State.Msg
    | ActionChange of Action.State.Msg
    | StatsChange of Stats.State.Msg
    | TestEvents

type Model =
  { Context: Context.Model
    State: State.Model}

type Msg =
  | ContextMsg of Context.Msg
  | StateMsg of State.Msg

[<RequireQualifiedAccess>] 
module Events =
  let update model (event: Events.Model) =
    let rec fold model result =
      match result with
      | Events.Compound results ->
        List.fold fold model results
      | Events.Msg log ->
        { model with State = { model.State with Logs = log::model.State.Logs } }
      | Events.Unlocks id ->
        let action =
          Action.allActions
          |> List.filter ((=) id << Action.State.id)
        { model with State = { model.State with Actions = model.State.Actions@action } }
    
    fold model event.Result

let init () =
  { Context = Context.init ()
    State = State.init () }
  , Cmd.ofMsg (StateMsg State.GetSave)
