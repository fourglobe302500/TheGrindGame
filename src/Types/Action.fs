[<RequireQualifiedAccess>]
module TGG.Types.Action

open TGG.Types
open Fable.React
open Elmish
open System

type Type =
  | Gathering
  | Combat
  | Exploration
  | Crafting
  | Farming
  | Hauling

[<RequireQualifiedAccess>]
module Type =
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

[<Measure>] type id

[<RequireQualifiedAccess>]
module State =
  type Model = 
    { Name: string
      Requirements: Item.Requirement list
      Results: Item.Result list
      Duration: Time
      Type: Type
      Id: int<id> }

  type Msg =
    | Run of Id: int<id>

  let Default = 
    { Name = "" 
      Requirements = []
      Results = []
      Duration = Seconds 0<sec>
      Type = Gathering
      Id = 0<id>}

  type Actions = Model list

  let name action = action.Name
  let requirements action = action.Requirements
  let results action = action.Results
  let duration action = action.Duration
  let type' action = action.Type
  let id action = action.Id

  let private format l f =
    l |> List.map (str << f)

  let private plural c = if c = 1 then "" else "s"

  let formatRequirements items action = 
    fun (Item.ItemRequirement(Item.ItemAmount(id, count))) 
      -> sprintf "Requires %i %s%s" count (Item.fromIdToString items id) (plural count)
    |> format action.Requirements

  let formatResults items action = 
    (fun (Item.ItemResult(Item.ItemAmount(id, count), chance)) -> 
      let withChance = 
        if chance = 100.<Item.percent> then ""
        else sprintf " with \na chance of %.1f%%" chance
      sprintf "Results %i %s%s%s" count (Item.fromIdToString items id) (plural count) withChance)
    |> format action.Results

[<RequireQualifiedAccess>]
module Context =
  type Model = ActionContext of Map<Type, bool>
  
  type Msg =
    | Toogle of Type

  let get s (ActionContext model) =
    model
    |> Map.find s

  let init () =
    Type.all
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

type Msg =
  | ContextMsg of Context.Msg
  | StateMsg of State.Msg

let canRun (inv: Inventory.State) (action: State.Model) =
  let requirementsMet =
    action.Requirements
    |> List.forall (fun (Item.ItemRequirement(Item.ItemAmount(item, count1))) -> (
      inv.Items
      |> List.map Item.Slot.get
      |> List.filter (fst >> Item.getId >> (=) item)
      |> List.exists (fun (_, count2) -> count2 >= count1) ) )
  let resultsMet =
    action.Results
    |> List.forall (fun (Item.ItemResult(Item.ItemAmount(item, _), _)) -> (
      let item = 
        inv.Items
        |> List.map Item.Slot.get
        |> List.filter (fst >> Item.getId >> (=) item)
      match item with
      | [] -> true
      | items -> List.exists (fun (_, count2) -> count2 < inv.MaxCap) items ) )
    
  requirementsMet && resultsMet

let requirement item c = Item.ItemRequirement << Item.ItemAmount <| (item, c)
let result item c (chance) = Item.ItemResult <| (Item.ItemAmount (item, c), chance*1.<Item.percent>) 

let initialActions = 
  let pebbleId = 0<Item.id>
  let stickId = 1<Item.id>
  
  [ { State.Default with
        Name = "Get Pebble"
        Type = Gathering
        Requirements = []
        Results = [
          result pebbleId 1 100 ]
        Duration = Seconds 5<sec>
        Id = 0<id> }
    { Type = Crafting
      Name = "Hit Peebles"
      Requirements = [
        requirement pebbleId 2 ]
      Results = [
        result pebbleId 1 100
        result pebbleId 1 80 ]
      Duration = Seconds 100<sec>
      Id = 1<id> }
    { Type = Gathering
      Name = "Get Stick"
      Requirements = []
      Results = [
        result stickId 1 95 ] 
      Duration = Seconds 5<sec>
      Id = 2<id> } ]
  |> fun l ->
    l
    |> List.map State.id
    |> List.countBy ((/) 1<id>)
    |> List.exists (snd >> (>) 1)
    |> function
      | false -> ()
      | true -> raise << Exception <| "Actions has dupped id"
    l