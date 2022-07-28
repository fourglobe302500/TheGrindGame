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
    let (<<|) f v = Result.map f v

    let (|=>) v f = Result.bind f v
    let (<=|) f v = Result.bind f v

    let map1 f (a, b) = f a, b
    let map2 f (a, b) = a, f b
    let tuple a b = a, b
    let triple a b c = a, b, c
    let rev f v2 v1 = f v1 v2
    let cons v l = v::l

    let uncurry f (v1, v2) = f v1 v2

    let (.|>) v f = map1 f v
    let (|>.) v f = map1 f v
    let (.<|) f v = map1 f v
    let (<|.) f v = map1 f v


    let (.|>>) v f = (Result.map << map1) f v
    let (|>>.) v f = (Result.map << map2) f v
    let (.<<|) f v = (Result.map << map1) f v
    let (<<|.) f v = (Result.map << map2) f v

[<RequireQualifiedAccess>]
module Log =
    type Message = Message of string

    type Msg =
    | Log of Message
    | ClearLogs

    let get = function Message s -> s
