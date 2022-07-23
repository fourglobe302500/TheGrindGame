[<RequireQualifiedAccess>]
module TGG.Core.Helpers

let inList a = [ a ]

let prettyItemsLog log (label: string) l (map: 'a -> 'b) = 
    let a =
        l
        |> List.map map
        |> List.toArray
  
    log(label, a)