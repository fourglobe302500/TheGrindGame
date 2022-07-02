[<RequireQualifiedAccess>]
module Action

open TGG.Types
open Fable.React
open Elmish

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

  let formatRequirements action = format "Needs" action.Requirements Item.Requirement.get 

  let formatResults action = format "Gives" action.Results Item.Result.get 

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

let canRun (inv: Inventory.State) (action: State.Model) =
  action.Requirements
  |> List.forall (fun (Item.ItemRequirement(Item.ItemAmount(item, count1))) -> (
    inv.Items
    |> List.map Slot.get
    |> List.filter (fst >> (=) item)
    |> List.exists (fun (_, count2) -> count2 >= count1) ) )
