namespace TGG.Core.Types

[<AutoOpen>]
module Global =
    type Result<'s, 'f> =
        | Success of 's
        | Failure of 'f

    [<RequireQualifiedAccess>]
    module Result =
        let bind f v =
            match v with
            | Failure f -> Failure f
            | Success s -> f s

        let map f v = bind <| (f >> Success) <| v

    let (|>>) v f = Result.map f v

    let (|=>) v f = Result.bind f v

[<RequireQualifiedAccess>]
module Log =
    type Message = Message of string

    type Msg =
    | Log of Message
    | ClearLogs

    let get = function Message s -> s
