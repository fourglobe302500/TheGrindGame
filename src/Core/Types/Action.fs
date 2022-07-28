[<RequireQualifiedAccess>]
module TGG.Core.Types.Action

open TGG.Core.Types
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

type Action =
    { Name: string
      Requirements: Item.Requirement list
      Results: Item.Result list
      Duration: Time
      Type: Type
      Id: int<id>
      Show: bool }

[<RequireQualifiedAccess>]
module Action =
    let Default = 
        { Name = "" 
          Requirements = []
          Results = []
          Duration = Seconds 0<sec>
          Type = Gathering
          Id = 0<id>
          Show = false }

    let name action = action.Name
    let requirements action = action.Requirements
    let results action = action.Results
    let duration action = action.Duration
    let type' action = action.Type
    let id action = action.Id
    let show action = action.Show

    let private format l f =
        l |> List.map (f)

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

    let canRun (inv: Inventory.Model) (action: Action) =
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

type Model = 
    { Actions: Action list
      Types: Map<Type, bool>
      IgnoreSpecific: bool }

type Msg =
| ToogleType of Type
| ToogleIgnore
| Run of int<id>

let getTypeVis s model =
    model.Types
    |> Map.find s

let requirement item c = Item.ItemRequirement << Item.ItemAmount <| (item, c)
let result item c (chance) = Item.ItemResult <| (Item.ItemAmount (item, c), chance*1.<Item.percent>) 

let allActions = 
    let pebbleId = 0<Item.id>
    let stickId = 1<Item.id>
    
    [ { Name = "Get Pebble"
        Type = Gathering
        Requirements = []
        Results = [
        result pebbleId 1 100 ]
        Duration = Seconds 5<sec>
        Id = 0<id>
        Show = false }
      { Type = Gathering
        Name = "Get Stick"
        Requirements = []
        Results = [
            result stickId 1 95 ] 
        Duration = Seconds 5<sec>
        Id = 1<id>
        Show = false }
      { Type = Crafting
        Name = "Hit Peebles"
        Requirements = [
            requirement pebbleId 2 ]
        Results = [
            result pebbleId 1 100
            result pebbleId 1 80 ]
        Duration = Seconds 100<sec>
        Id = 2<id>
        Show = false } ]
    |> fun l ->
        l
        |> List.map Action.id
        |> List.countBy ((/) 1<id>)
        |> List.exists (snd >> (>) 1)
        |> function
        | false -> ()
        | true -> raise << Exception <| "Actions has dupped id"
        l

let innitialActions = 
    allActions.[0..1]

let init () =
    { Actions = innitialActions
      Types =
        Type.all
        |> List.map (fun t -> t,false)
        |> Map.ofList
      IgnoreSpecific = false }