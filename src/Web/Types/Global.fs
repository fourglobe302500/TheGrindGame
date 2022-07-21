[<AutoOpen>]
module TGG.Types.Global

type Result<'s, 'f> =
  | Success of 's
  | Failure of 'f

type Log = Message of string

[<RequireQualifiedAccess>]
module Log =
  let get = function Message s -> s
