[<RequireQualifiedAccess>]
module TGG.Core.Types.Events

open TGG.Core.Types

[<Measure>] type id

type Condition =
| OrCondition of Condition list
| HasItems of Item.Amount list
| ItemCount of Item.Amount list
| ActionCount of ActionId: int<Action.id> * Count: int

[<RequireQualifiedAccess>]
module Condition =
    let rec isMet inv (stats: Stats.Model) = function
    | OrCondition conditions -> List.exists (isMet inv stats) conditions
    | HasItems amounts -> 
        amounts
        |> List.map Item.Amount.get
        |> List.forall (fun (id, amount) ->
                            inv
                            |> List.map Item.Slot.get
                            |> List.filter ((=) id << Item.getId << fst)
                            |> List.exists ((<=) amount << snd) )
    | ItemCount count -> 
        count
        |> List.map Item.Amount.get
        |> List.forall (fun (id, amount) ->
                            stats.ItemsMade
                            |> Stats.get
                            |> List.filter ((=) id << fst)
                            |> List.exists ((<=) amount << snd) )
    | ActionCount (id, count) ->
        stats.Actions
        |> Stats.get
        |> List.filter ((=) id << fst)
        |> List.exists ((<=) count << snd)
                  
type EventResult =
| Msg of Log.Message
| Unlocks of int<Action.id>

type Model =
    { Conditions: Condition list
      Result: EventResult list
      Id: int<id>}

type Msg = 
| MatchEvents

let isMet {Inventory.Items = items} stats ({ Conditions = conditions }) =
    conditions |> List.forall (Condition.isMet items stats)

let events = 
    [ { Id = 0<id>
        Conditions = [ HasItems [ Item.ItemAmount (0<Item.id>, 2) ] ] 
        Result = 
            [ Msg << Log.Message <| "You think of the stupid idea of hitting two pebbles togeter"
              Unlocks 2<Action.id> ] } ]