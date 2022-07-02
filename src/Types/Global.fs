[<AutoOpen>]
module TGG.Types

open Fable
open Fable.React
open Elmish

type Result<'s, 'f> =
  | Success of 's
  | Failure of 'f

type Item = 
  | Empty
  | Pebble
  | Stick

[<RequireQualifiedAccess>]
module Item =
  [<Measure>] type id

  let toString = function
    | Empty -> "Empty"
    | Pebble -> "Pebble"
    | Stick -> "Stick"

  let fromString = function
    | "Pebble" -> Pebble
    | "Stick" -> Stick
    | _ -> Empty

  let fromId = function
    | 1<id> -> Pebble
    | 2<id> -> Stick
    | _ -> Empty

  let getId = function
    | Empty -> 0<id>
    | Pebble -> 1<id>
    | Stick -> 2<id>

  let (|GetItem|) = fromString

  let (|Item|) = toString

type SaveName =
  | Save of string
  | Empty

[<RequireQualifiedAccess>]
module SaveName =
  let get = function | Save (name) -> name | Empty -> ""

type Slot = Slot of Item: Item * Count: int

[<RequireQualifiedAccess>]
module Slot =
  let get = function Slot(item, count) -> item, count

type InventoryState =
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
      InventoryState.manipulate (fun count -> max (count+1) inv.MaxCap) inv item

    static member (-) (inv, item) =
      InventoryState.manipulate (fun count -> min (count-1) 0) inv item

    static member canRemove item m = 
      m.Items
      |> List.map (Slot.get >> fst)
      |> List.contains item

type Log = Message of string

[<RequireQualifiedAccess>]
module Log =
  let get = function Message s -> s

type ItemAmount = ItemAmount of item: Item * count: int

[<RequireQualifiedAccess>]
module ItemAmount =
  let get = function ItemAmount (item, count) -> item,count

type ItemRequirement = ItemRequirement of ItemAmount

[<RequireQualifiedAccess>]
module ItemRequirement =
  let get = function ItemRequirement a -> ItemAmount.get a

type ItemResult = ItemResult of ItemAmount

[<RequireQualifiedAccess>]
module ItemResult =
  let get = function ItemResult a -> ItemAmount.get a

[<Measure>] type sec

[<Measure>] type min

[<Measure>] type h

[<Measure>] type day

[<Measure>] type y

type Time =
  | Seconds of int<sec>
  | Minutes of int<min>
  | Hours of int<h>
  | Days of int<day>
  | Years of int<y>

[<RequireQualifiedAccess>]
module Time =
  let secPerMin = 60<sec/min>
  let getMin (sec: int<sec>) = sec % 60<sec>, sec/secPerMin
  let (|GetMin|) = getMin

  let minPerHour = 60<min/h>
  let getHour (min: int<min>) = min % 60<min>, min/minPerHour
  let (|GetHour|) = getHour

  let hourPerDay = 24<h/day>
  let getDay (h: int<h>) = h % 24<h>, h/hourPerDay
  let (|GetDay|) = getDay

  let daysPerYear = 365<day/y>
  let getYear (day: int<day>) = day % 365<day>, day/daysPerYear
  let (|GetYear|) = getYear

  let (|GetAllFromSec|) = function (GetMin (s, GetHour (m, GetDay (h, GetYear (d, y))))) -> (s, m, h, d, y) 
  let (|GetAllFromMin|) = function ( GetHour (m, GetDay (h, GetYear (d, y)))) -> (m, h, d, y) 
  let (|GetAllFromHour|) = function (GetDay (h, GetYear (d, y))) -> (h, d, y)  

  let get = function
    | Seconds (GetAllFromSec (s, m, h, d, y)) -> (y, d, h, m, s) 
    | Minutes (GetAllFromMin (m, h, d, y)) -> (y, d, h, m, 0<sec>)
    | Hours (GetAllFromHour (h, d, y)) -> (y, d, h, 0<min>, 0<sec>)
    | Days (GetYear (d, y)) -> (y, d, 0<h>, 0<min>, 0<sec>)
    | Years y -> (y, 0<day>, 0<h>, 0<min>, 0<sec>)

  let fromAllToText (y, d, h, m, s) =
    sprintf "%i:%i:%i, day %i, year %i" h m s d y

  let fromAllToDuration (y, d, h, m, s) =
    let s = if s = 0<sec> then None else Some <| sprintf "%i seconds" s 
    let m = if m = 0<min> then None else Some <| sprintf "%i minutes" m 
    let h = if h = 0<h> then None else Some <| sprintf "%i hours" h 
    let d = if d = 0<day> then None else Some <| sprintf "%i days" d 
    let y = if y = 0<y> then None else Some <| sprintf "%i years" y

    [y; d; h; m; s]
    |> List.filter Option.isSome
    |> List.map (fun op -> op.Value)
    |> String.concat ", "

  let toText = get >> fromAllToText

  let toDuration = get >> fromAllToDuration

  let toTextFromSeconds = Seconds >> toText

  let toDurationFromSeconds = Seconds >> toDuration

type ActionType =
  | Gathering
  | Combat
  | Exploration
  | Crafting
  | Farming
  | Hauling

[<RequireQualifiedAccess>]
module ActionType =
  let get = function
    | Gathering -> "Gathering"
    | Combat -> "Combat"
    | Exploration -> "Exploration"
    | Crafting -> "Crafting"
    | Farming -> "Farming"
    | Hauling -> "Hauling"

  let all = [
    Gathering
    Combat
    Exploration
    Crafting
    Farming
    Hauling ]

[<RequireQualifiedAccess>]
module Action =
  [<Measure>] type id

  [<RequireQualifiedAccess>]
  module State =

    type Model = 
      { Name: string
        Requirements: ItemRequirement list
        Results: ItemResult list
        Duration: Time
        Type: ActionType
        Id: int<id> }

    type Actions = Model list

    let name action = action.Name
    let requirements action = action.Requirements
    let results action = action.Results
    let duration action = action.Duration
    let type' action = action.Type
    let id action = action.Id

    let private format t l get =
      if List.length l = 0 then str "No Requirements"
      else
        l
        |> List.map (fun e ->
          let (item, count) = get e
          str <| sprintf "%s %i %s%s" t count (Item.toString item) (if count = 0 then "" else "s"))
        |> fragment []

    let formatRequirements action = format "Needs" action.Requirements ItemRequirement.get 

    let formatResults action = format "Gives" action.Results ItemResult.get 

  [<RequireQualifiedAccess>]
  module Context =
    type Model = ActionContext of Map<ActionType, bool>
    
    type Msg =
      | Toogle of ActionType

    let get s (ActionContext model) =
      model
      |> Map.find s

    let init () =
      ActionType.all
      |> List.map (fun t -> t,false)
      |> Map.ofList
      |> ActionContext

    let update msg (ActionContext model) =
      match msg with
      | Toogle t ->
        model
        |> Map.change t (Option.map (fun b -> not b))
        |> ActionContext
        , Cmd.none

[<RequireQualifiedAccess>]
module SaveContext =
  type Model = 
    { AskSave: bool
      InputValue: string
      Save: bool
      FileOver: bool }

  type Msg =
    | AskSaveToogle
    | ToogleFileOver
    | InputChange of string
    
  let init () = 
    { AskSave = true
      InputValue = ""
      Save = false
      FileOver = false }

[<RequireQualifiedAccess>]
module AppContext =
  type Model = 
    { Save: SaveContext.Model
      Actions: Action.Context.Model }

  type Msg =
    | SaveContextChange of SaveContext.Msg
    | ActionContextChange of Action.Context.Msg

  let init () =
    { Save = SaveContext.init()
      Actions = Action.Context.init() }

open Browser
open Fable.SimpleJson
open Fable.Core
open Elmish

[<RequireQualifiedAccess>]
module AppState =
  type Model = 
    { Inventory: InventoryState 
      Logs: Log list
      Actions: Action.State.Actions
      SaveName: SaveName
      Time: int<sec> }

  let init () = 
    { Time = 0<sec>
      SaveName = Empty
      Actions = [
        {
          Type = Gathering
          Name = "Get Pebble"
          Requirements = []
          Results = [
            ItemResult <| ItemAmount (Pebble, 1)
          ]
          Duration = Seconds 5<sec>
          Id = 0<Action.id>
        }
        {
          Type = Crafting
          Name = "Hit Peebles"
          Requirements = [
            ItemRequirement <| ItemAmount (Pebble, 2)
            ItemRequirement <| ItemAmount (Stick, 1)
          ]
          Results = [
            ItemResult <| ItemAmount (Pebble, 2)
          ]
          Duration = Seconds 100<sec>
          Id = 1<Action.id>
        }
      ]
      Logs = []
      Inventory = { Items = [
        Slot(Pebble, 5)
        Slot(Stick, 5)
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

  type ReturnMsg =
    | Context of AppContext.Msg
    | State of Msg

  let update msg model =
    match msg with
    | StoreSave ->
      let json = SimpleJson.stringify model
      console.log("Saving", json)
      localStorage.setItem("save", json)
      model, Cmd.none
    | GetSave ->
      let json = localStorage.getItem("save")
      model, Cmd.ofMsg (State <| LoadSave json)
    | AddItem item ->
      let inventory = model.Inventory + item
      { model with Inventory = inventory }, Cmd.none
    | RemoveItem item ->
      let inventory = model.Inventory - item
      { model with Inventory = inventory }, Cmd.none
    | ChangeMaxCapBy delta ->
      if delta > 0 then
        { model with Inventory = { model.Inventory with MaxCap = model.Inventory.MaxCap+delta } }, Cmd.none
      else model, Cmd.none
    | ChangeMaxCapTo maxCap ->
      if maxCap > 0 then
        { model with Inventory = { model.Inventory with MaxCap = maxCap } }, Cmd.none
      else model, Cmd.none
    | LoadSave json ->
      console.log("Loading", json)
      if Helpers.notEmpty json then
        let model = Json.parseAs<Model> json
        console.log("Loaded", model)
        model, 
          match model.SaveName with 
          | Empty -> Cmd.none 
          | _ -> Cmd.ofMsg (Context << AppContext.Msg.SaveContextChange <| SaveContext.Msg.AskSaveToogle)
      else model, Cmd.ofMsg (State StoreSave)
    | ChangeSaveName saveName -> 
      (if Helpers.notEmpty saveName then
        { model with SaveName = Save <| saveName.TrimEnd() }
      else
        { model with SaveName = Empty })
      , Cmd.ofMsg (State StoreSave)
    | WipeSave ->
      init(), Cmd.ofMsg (State StoreSave)
    | Log msg ->
      { model with Logs = Message(msg)::model.Logs }, Cmd.ofMsg (State StoreSave)
    | ClearLogs ->
      { model with Logs = [] }, Cmd.ofMsg (State StoreSave)

