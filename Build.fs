open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext()

let webPath = Path.getFullName "src/Web"
let deployPath = Path.getFullName "deploy"

Target.create "Clean" (fun _ ->
  Shell.cleanDir deployPath
  run dotnet "fable clean --yes" webPath // Delete *.fs.js files created by Fable
)

Target.create "InstallWeb" (fun _ -> run yarn "install" ".")

Target.create "Bundle" (fun _ ->
  [ "web", dotnet "fable -o output -s --run yarn build" webPath ]
  |> runParallel
)

Target.create "Run" (fun _ ->
  [ "web", dotnet "fable watch -o output -s --run yarn start" webPath ]
  |> runParallel
)

open Fake.Core.TargetOperators

let depedencies = [
  "Clean"
    ==> "InstallWeb"
    ==> "Bundle"

  "Clean"
    ==> "InstallWeb"
    ==> "Run"
]

[<EntryPoint>]
let main args = runOrDefault args