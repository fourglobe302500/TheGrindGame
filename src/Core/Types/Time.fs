[<AutoOpen>]
module TGG.Core.Types.Time

[<Measure>] type sec

[<Measure>] type min

[<Measure>] type h

[<Measure>] type day

[<Measure>] type y

type Time =
    | Seconds of int<sec>
    | Minutes of int<min>
    | Hours of int<h>
    | Days of int<day>
    | Years of int<y>

[<RequireQualifiedAccess>]
module Time =
    let secPerMin = 60<sec/min>
    let getMin (sec: int<sec>) = sec % 60<sec>, sec/secPerMin
    let (|GetMin|) = getMin

    let minPerHour = 60<min/h>
    let getHour (min: int<min>) = min % 60<min>, min/minPerHour
    let (|GetHour|) = getHour

    let hourPerDay = 24<h/day>
    let getDay (h: int<h>) = h % 24<h>, h/hourPerDay
    let (|GetDay|) = getDay

    let daysPerYear = 365<day/y>
    let getYear (day: int<day>) = day % 365<day>, day/daysPerYear
    let (|GetYear|) = getYear

    let (|GetAllFromSec|) = function (GetMin (s, GetHour (m, GetDay (h, GetYear (d, y))))) -> (s, m, h, d, y) 
    let (|GetAllFromMin|) = function ( GetHour (m, GetDay (h, GetYear (d, y)))) -> (m, h, d, y) 
    let (|GetAllFromHour|) = function (GetDay (h, GetYear (d, y))) -> (h, d, y)  

    let toSec = function
    | Seconds t -> t
    | Minutes t -> t*secPerMin
    | Hours t -> t*secPerMin*minPerHour
    | Days t -> t*secPerMin*minPerHour*hourPerDay
    | Years t -> t*secPerMin*minPerHour*hourPerDay*daysPerYear

    let addTime s t =
        toSec t |> (+) s

    let get = function
    | Seconds (GetAllFromSec (s, m, h, d, y)) -> (y, d, h, m, s) 
    | Minutes (GetAllFromMin (m, h, d, y)) -> (y, d, h, m, 0<sec>)
    | Hours (GetAllFromHour (h, d, y)) -> (y, d, h, 0<min>, 0<sec>)
    | Days (GetYear (d, y)) -> (y, d, 0<h>, 0<min>, 0<sec>)
    | Years y -> (y, 0<day>, 0<h>, 0<min>, 0<sec>)

    let fromAllToText (y, d, h, m, s) =
        sprintf "%02i:%02i:%02i, day %i, year %i" h m s d y

    let fromAllToDuration (y, d, h, m, s) = 
        let s = if s = 0<sec> then None else Some <| sprintf "%i seconds" s 
        let m = if m = 0<min> then None else Some <| sprintf "%i minutes" m 
        let h = if h = 0<h> then None else Some <| sprintf "%i hours" h 
        let d = if d = 0<day> then None else Some <| sprintf "%i days" d 
        let y = if y = 0<y> then None else Some <| sprintf "%i years" y

        [y; d; h; m; s]
        |> List.filter Option.isSome
        |> List.map (fun op -> op.Value)
        |> String.concat ", "

    let toText = get >> fromAllToText

    let toDuration = get >> fromAllToDuration

    let toTextFromSeconds = Seconds >> toText

    let toDurationFromSeconds = Seconds >> toDuration