[<RequireQualifiedAccess>]
module App

open TGG
open TGG.Types
open Fable
open Fable.React
open Fable.SimpleJson
open Elmish
open Browser

[<RequireQualifiedAccess>]
module Context =
  type Model = 
    { Save: Save.Context.Model
      Actions: Action.Context.Model }

  type Msg =
    | SaveContextChange of Save.Context.Msg
    | ActionContextChange of Action.Context.Msg

  let init () =
    { Save = Save.Context.init()
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

let update msg (model: Model) =
  match msg with
  | ContextMsg msg ->
    match msg with
    | Context.SaveContextChange msg -> 
      let (saveModel, cmd) = Save.update msg model.Context.Save
      { model with Context = { model.Context with Save = saveModel } }, Cmd.map ContextMsg cmd
    | Context.ActionContextChange msg ->
      let (actionModel, cmd) = Action.Context.update msg model.Context.Actions
      { model with Context = { model.Context with Actions = actionModel } }, Cmd.map ContextMsg cmd
  | StateMsg msg ->
    match msg with
    | State.StoreSave ->
      let json = SimpleJson.stringify model
      console.log("Saving", json)
      localStorage.setItem("save", json)
      model, Cmd.none
    | State.GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (StateMsg <| State.LoadSave json)
    | State.AddItem item ->
      let inventory = model.State.Inventory + item
      { model with State = { model.State with Inventory = inventory } }, Cmd.none
    | State.RemoveItem item ->
      let inventory = model.State.Inventory - item
      { model with State = { model.State with Inventory = inventory } }, Cmd.none
    | State.ChangeMaxCapBy delta ->
      if delta > 0 then
        { model with State = { model.State with Inventory = { model.State.Inventory with MaxCap = model.State.Inventory.MaxCap+delta } } }, Cmd.none
      else model, Cmd.none
    | State.ChangeMaxCapTo maxCap ->
      if maxCap > 0 then
        { model with State = { model.State with Inventory = { model.State.Inventory with MaxCap = maxCap } } }, Cmd.none
      else model, Cmd.none
    | State.LoadSave json ->
      console.log("Loading", json)
      if Helpers.notEmpty json then
        let state = Json.parseAs<State.Model> json
        console.log("Loaded", model)
        { model with State = state }, 
          match model.State.SaveName with 
          | Save.Empty -> Cmd.none 
          | _ -> Cmd.ofMsg (ContextMsg << Context.Msg.SaveContextChange <| Save.Context.Msg.AskSaveToogle)
      else model, Cmd.ofMsg (StateMsg State.StoreSave)
    | State.ChangeSaveName saveName -> 
      (if Helpers.notEmpty saveName then
        { model with State = { model.State with SaveName = Save.Save <| saveName.TrimEnd() } }
      else
        { model with State = { model.State with SaveName = Save.Empty } } )
      , Cmd.ofMsg (StateMsg State.StoreSave)
    | State.WipeSave ->
      { model with State =  State.init() } , Cmd.ofMsg (StateMsg State.StoreSave)
    | State.Log msg ->
      { model with State = { model.State with Logs = Message(msg)::model.State.Logs } }, Cmd.ofMsg (StateMsg State.StoreSave)
    | State.ClearLogs ->
      { model with State = { model.State with Logs = [] } }, Cmd.ofMsg (StateMsg State.StoreSave)