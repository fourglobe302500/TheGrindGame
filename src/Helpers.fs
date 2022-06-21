[<RequireQualifiedAccess>]
module TGG.Helpers

open Fable.SimpleJson

let private parseJsonObj format mapper = function
  | (JObject dict) -> 
    let value key = Map.tryFind key dict
    format
    |> List.map value
    |> List.choose id
    |> mapper
  | _ -> None

let (|ParseJsonObj|_|) format json=
  match json with
  | (JObject dict) -> 
    let value key = Map.tryFind key dict
    format
    |> List.map value
    |> List.choose id
    |> Some
  | _ -> None

let private parseJsonArray (mapper: 'a -> 'b option) l =
  let res = List.map (mapper) l
  if List.forall (function
    | Some x -> true
    | None -> false) res then
    res |> List.map (fun opt -> opt.Value) |> Some
  else None 

let (|ParseJsonArray|_|) mapper json =
  match json with
  | (JArray l) -> 
    parseJsonArray mapper l
  | _ -> None
