module TGG.Web.App

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop
open Fable.SimpleJson

open Browser

open Elmish
open Elmish.React
open Elmish.ReactNative

#if DEBUG
open Elmish.Debug
#endif

open TGG.Web.Components
open TGG.Core.Types
open TGG.Core.Compiling

importAll "./App.scss"

let view (model: App.Model) (dispatch: Dispatch<App.Msg>) =
    fragment [] [
        Save.view model.Save dispatch 
        Stats.view model (dispatch << App.StatsChange)

        div [ Class "container" ] [
        div [ Class "header"  ] [
            h1 [Class "title"] [ str "The Grind Game"]
            h3 [ Class "time" ] [ str << Time.toTextFromSeconds <| model.Time ]
            div [ Class "right" ] [
            h2 [ Class "stats" ] [ 
                span 
                    [ OnClick (fun _ -> dispatch << App.StatsChange <| Stats.StatsShowToogle ) ] 
                    [ str "Stats" ] ]
            h2 [ Class "save-name" ] [ 
                span 
                    [ OnClick (fun _ -> dispatch << App.SaveChange <| Save.AskSaveToogle ) ]
                    [ match model.Save.SaveName with
                        | Save.Save saveName -> 
                            str saveName
                        | Save.Empty -> 
                            str "No Save" ] ] ] ]
        div [ Class "body"  ] [
            Inventory.view model.Inventory
            Action.view model dispatch
            div [ Class "automation-logger-container" ] [
            div [ Class "automation" ] [] 
            div [ Class "logger" ] [
                for log in model.Logs -> 
                span [ Class "log" ] [ str <| Log.get log ] ] ] ] ] ]

let handle msg model: App.Model * Cmd<App.Msg> =
    match msg with
    | App.StoreSave ->
        let json = SimpleJson.stringify model
        localStorage.setItem("fgtgg-save", json)
        model, Cmd.none
    | App.GetSave ->
        let json = localStorage.getItem("fgtgg-save")
        model, Cmd.ofMsg (App.Local << App.LoadSave <| json)
    | App.LoadSave json ->
        if Helpers.notEmpty json then
            let model = Json.parseAs<App.Model> json
            model, 
            match model.Save.SaveName with 
            | Save.Empty -> Cmd.ofMsg << App.SaveChange << Save.AskSaveSet <| true
            | _ -> Cmd.ofMsg << App.SaveChange << Save.AskSaveSet <| false
        else 
            model, 
            Cmd.batch <| [
                Cmd.ofMsg << App.Local <| App.StoreSave
                Cmd.ofMsg << App.SaveChange <| Save.AskSaveToogle ]

"run action test ignores Pebble"
|> Parser.parse
|> function
| Global.Failure f -> printfn "%s" f
| Global.Success tokens ->
List.map (sprintf "%O") tokens
|> List.toArray
|> fun l -> console.log(l)

Program.mkProgram App.init (App.update handle) view
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run