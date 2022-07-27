namespace TGG.Core.Types

[<RequireQualifiedAccess>]
module Global =
    type Result<'s, 'f> =
        | Success of 's
        | Failure of 'f

[<RequireQualifiedAccess>]
module Log =
    type Message = Message of string

    type Msg =
    | Log of Message
    | ClearLogs

    let get = function Message s -> s
