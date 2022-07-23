open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext()

let webPath = Path.getFullName "src/Web"
let corePath = Path.getFullName "src/Core"
let deployPath = Path.getFullName "deploy/public"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet "fable clean --yes" webPath // Delete *.fs.js files created by Fable
)

Target.create "BuildCore" (fun _ -> run dotnet "build" corePath)

Target.create "InstallWeb" (fun _ -> run yarn "install" ".")

Target.create "Bundle" (fun _ ->
    [ "web", dotnet "fable -o output -s --run yarn build" webPath ]
    |> runParallel
)

Target.create "Run" (fun _ ->
    // Shell.copyFile 
    //     (Path.combine deployPath "index.html")    
    //     (Path.combine webPath "index.html") 
    [ "web", dotnet "fable watch -o output -s --run yarn start" webPath ]
    |> runParallel
)

Target.create "Deploy" (fun _ -> run yarn "deploy" ".")

open Fake.Core.TargetOperators

let depedencies = [
    "Clean"
        ==> "InstallWeb"
        ==> "BuildCore"
        ==> "Bundle"

    "Clean"
        ==> "InstallWeb"
        ==> "BuildCore"
        ==> "Run"

    "Clean"
        ==> "InstallWeb"
        ==> "BuildCore"
        ==> "Bundle"
        ==> "Deploy"
]

[<EntryPoint>]
let main args = runOrDefault args