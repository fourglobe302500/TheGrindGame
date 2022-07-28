[<RequireQualifiedAccess>]
module TGG.Core.Types.App

open TGG.Core.Types
open Elmish

type Model =
    { Save: Save.Model
      Time: int<sec>
      Actions: Action.Model
      Stats: Stats.Model
      Inventory: Inventory.Model
      Logs: Log.Message list
      Events: Events.Model list }

type Local =
| StoreSave
| GetSave
| LoadSave of Json: string

type Msg =
// Save
| Local of Local
| WipeSave

// State and UI Changes
| LogChange of Log.Msg
| SaveChange of Save.Msg
| InventoryChange of Inventory.Msg
| ActionChange of Action.Msg
| StatsChange of Stats.Msg
| EventsChange of Events.Msg


let init () =
    { Save = Save.init ()
      Time = 0<sec>
      Actions = Action.init ()
      Stats = Stats.init ()
      Inventory = Inventory.init ()
      Logs = []
      Events = [] }
    , Cmd.ofMsg << Local <| GetSave

module private Update =
    let save msg (model: Save.Model) =
        match msg with
        | Save.AskSaveToogle -> { model with AskSave = not model.AskSave }
        | Save.AskSaveSet v -> { model with AskSave = v }
        | Save.ToogleFileOver -> { model with FileOver = not model.FileOver}
        | Save.InputChange v -> { model with InputValue = v}
        | Save.ChangeSaveName v -> { model with SaveName = v }

    let action (msg: Action.Msg) model =
        match msg with
        | Action.ToogleType t ->
            { model with
                Actions = 
                    { model.Actions with
                        Types =
                            model.Actions.Types
                            |> Map.change t (Option.map (fun b -> not b)) } }, Cmd.ofMsg << Local <| StoreSave
        | Action.ToogleIgnore -> { model with Actions = { model.Actions with IgnoreSpecific = not model.Actions.IgnoreSpecific } }, Cmd.ofMsg << Local <| StoreSave
        | Action.Run id ->
            model.Actions.Actions
            |> List.tryFind (Action.Action.id >> (=) id)
            |> function
            | None -> model, Cmd.none
            | Some action ->
            if not <| Action.Action.canRun model.Inventory action then model, Cmd.none
            else
            action.Requirements
            |> List.map (Item.Requirement.get)
            |> List.fold (fun inv (id, amount) -> inv -- (Item.fromId id Item.items, amount)) model.Inventory
            |> fun inv ->
            action.Results
            |> List.map (Item.Result.get)
            |> List.fold (fun inv (id, amount, chance) -> inv ++ (Item.fromId id Item.items, amount, chance)) inv
            |> fun finv ->
            let diff = Inventory.dif finv inv
            { model with
                Time = Time.addTime model.Time action.Duration 
                Inventory = finv },
            Cmd.batch [
                Cmd.ofMsg << Local <| StoreSave
                Cmd.ofMsg << StatsChange << Stats.RunAction <| (action.Id, diff)
                Cmd.ofMsg << EventsChange <| Events.MatchEvents ]

    let stats (msg: Stats.Msg) model = 
        match msg with
        | Stats.StatsShowToogle -> { model with Stats = { model.Stats with Show = not model.Stats.Show } }
        | Stats.RunAction (id, added) ->
            List.tryFind (Action.Action.id >> (=) id) model.Actions.Actions
            |> function
            | None -> model
            | Some action ->
            let actionsStats = 
                model.Stats.Actions
                |> Stats.get
                |> List.tryFind (fst >> (=) id)
                |> function
                | Some _ -> 
                    model.Stats.Actions
                    |> Stats.get
                    |> List.map (fun (id, c) -> if id = action.Id then (id, c+1) else (id, c))
                    |> Stats.Stat
                | None ->
                    model.Stats.Actions
                    |> Stats.get
                    |> fun l -> l@[(id, 1)]
                    |> Stats.Stat


            let itemsMade =
                let itemsMade = model.Stats.ItemsMade |> Stats.get

                itemsMade
                |> List.map fst
                |> List.append (List.map (Item.Slot.get >> fst >> Item.getId) added)
                |> List.distinct
                |> List.sort
                |> List.map (fun id ->
                let item1 =
                    itemsMade |> List.tryFind (fst >> (=) id)
                let item2 =
                    added |> List.map (Item.Slot.get) |> List.tryFind (fst >> Item.getId >> (=) id)

                match item1, item2 with
                | (Some (_, count1), Some (_, count2)) -> (id, count1+count2)
                | (Some (_, count), None) -> (id, count)
                | (None, Some (_, count)) -> (id, count)
                | (None, None) -> (id, 0) )
                |> Stats.Stat

            let itemsUsed =
                let itemsUsed = model.Stats.ItemsUsed |> Stats.get
                
                let itemsRequired =action.Requirements |> List.map Item.Requirement.get

                itemsUsed
                |> List.map fst
                |> List.append (List.map (fst) itemsRequired)
                |> List.distinct
                |> List.sort
                |> List.map (fun id ->
                let item1 =
                    itemsUsed |> List.tryFind (fst >> (=) id)
                let item2 =
                    itemsRequired |> List.tryFind (fst >> (=) id)
                match item1, item2 with
                | (Some (_, count1), Some (_, count2)) -> (id, count1+count2)
                | (Some (_, count), None) -> (id, count)
                | (None, Some (_, count)) -> (id, count)
                | (None, None) -> (id, 0) )
                |> Stats.Stat

            { model with
                Stats = {
                  model.Stats with
                    Actions = actionsStats
                    ItemsMade = itemsMade
                    ItemsUsed = itemsUsed } }


    let log (msg: Log.Msg) model = 
        match msg with 
        | Log.ClearLogs -> []
        | Log.Log log -> log::model

    let inventory (msg: Inventory.Msg) (inventory: Inventory.Model) = 
        match msg with
        | Inventory.AddItem    item -> inventory + item
        | Inventory.RemoveItem item -> inventory - item
        | Inventory.ChangeMaxCapBy delta ->
            if delta > 0 then
                { inventory with MaxCap = inventory.MaxCap + delta}
            else inventory
        | Inventory.ChangeMaxCapTo maxCap ->
            if maxCap > 0 then
                { inventory  with MaxCap = maxCap }
            else inventory

    let events (msg: Events.Msg) model = 
        match msg with
        | Events.MatchEvents ->
            model.Events
            |> List.partition (Events.isMet model.Inventory model.Stats)
            |> fun (areMet, aren'tMet) ->
                let up model (ev: Events.Model) =
                    let rec fold model result =
                        match result with
                        | Events.Msg log ->
                            { model with Logs = log::model.Logs }
                        | Events.Unlocks id ->
                            let action =
                                Action.allActions
                                |> List.filter ((=) id << Action.Action.id)
                            { model with Actions = { model.Actions with Actions = model.Actions.Actions@action } }
                    List.fold fold model ev.Result
                let model = areMet |> List.fold up model
                { model with Events = aren'tMet}

let update handle msg model : Model * Cmd<Msg> =
    match msg with
    | SaveChange msg -> 
        let save = Update.save msg model.Save
        { model with Save = save }, Cmd.ofMsg << Local <| StoreSave
    | LogChange msg ->
        { model with Logs = Update.log msg model.Logs }
        , Cmd.ofMsg << Local <| StoreSave
    | EventsChange msg ->
        Update.events msg model, Cmd.ofMsg << Local <| StoreSave
    | InventoryChange msg ->
        { model with Inventory = Update.inventory msg model.Inventory }
        , Cmd.ofMsg << Local <| StoreSave
    | ActionChange msg -> Update.action msg model
    | StatsChange msg ->
        Update.stats msg model, Cmd.ofMsg << Local <| StoreSave

    | Local msg -> handle msg model
    | WipeSave ->
        init () |> fst, Cmd.ofMsg << Local <| StoreSave