[<RequireQualifiedAccess>]
module TGG.Core.Parsing.Parser

open FParsec
open System

type ActionAccessor = ActionAccessor of string
type InventoryAccessor = InventoryAccessor of string
type ItemAccessor = ItemAccessor of string
type ActionRunner = ActionRunner of ActionAccessor * Ignores: ItemAccessor list
type Variable = 
| InInv of InventoryAccessor
| Const of int
type Comparison =
| GT of Variable * Variable
| GTE of Variable * Variable
| LT of Variable * Variable
| LTE of Variable * Variable
| EQ of Variable * Variable
| NEQ of Variable * Variable
type Bool =
| Comp of Comparison
| CanRun of ActionAccessor * Ignores: ItemAccessor list
type Statement =
| Block of Statement list
| Loop of count: int * Statement
| While of Bool * Statement
| If of Bool * Statement * Statement option
| Run of ActionRunner

let private str = pstring
let private ws = spaces

let private betweenQuotes = between (str "'") (str "'")
let private betweenBraces = between (str "[") (str "]")

let private either f1 f2 v = f1 v || f2 v

let private access =
    let stringWithWs = 
        let valid = either Char.IsLetter Char.IsWhiteSpace
        manyChars (satisfy valid)
    let string = many1Chars (satisfy Char.IsLetter)
    string <|> betweenQuotes stringWithWs

let private actionAccessorParser =
    pstring "action" >>. ws >>. access .>> ws |>> ActionAccessor

let private inventoryAccessorParser =
    pstring "inventory" >>. ws >>. access .>> ws |>> InventoryAccessor

let private itemAccessorParser =
    pstring "item" >>. ws >>. access .>> ws |>> ItemAccessor

let private oneOrList p =
    let list =
        sepBy1 p (str "," >>. ws)
        |> betweenBraces
    let single = p |>> fun v -> [v]
    single <|> list

let private variable = 
    let number = pint32 |>> Const
    let inInv = inventoryAccessorParser |>> InInv
    number <|> inInv

let private gt = variable .>> ws .>> str ">" .>> ws .>>. variable |>> GT
let private gte = variable .>> ws .>> str ">=" .>> ws .>>. variable |>> GTE
let private lt = variable .>> ws .>> str "<" .>> ws .>>. variable |>> LT
let private lte = variable .>> ws .>> str "<=" .>> ws .>>. variable |>> LTE
let private eq = variable .>> ws .>> str "==" .>> ws .>>. variable |>> EQ
let private neq = variable .>> ws .>> str "!=" .>> ws .>>. variable |>> EQ

let private comparison =
    [ gt; gte; lt; lte; eq; neq ]
    |> List.map attempt
    |> choice

let private ignoresClause =
    str "ignores" >>. ws >>. oneOrList itemAccessorParser
    |> opt
    |>> function | None -> [] | Some v -> v

let private canRunParser =
    str "canRun" >>. ws >>. actionAccessorParser .>> ws .>>. ignoresClause |>> CanRun

let private boolParser =
    choice [
        canRunParser
        comparison |>> Comp
    ]

let private runAction =
    str "run" >>. ws >>. actionAccessorParser .>> ws .>>. ignoresClause |>> ActionRunner

let private statementParser, statementParserRef = createParserForwardedToRef()

let private runParser = runAction |>> Run

let private blockParser = between (str "{") (str "}") (many statementParser) |>> Block

let private ifParser = 
    let else' = str "else" >>. ws >>. statementParser |> opt

    str "if" >>. ws >>. boolParser .>>. statementParser .>> ws .>>. else'
    |>> fun ((b, i), e) -> If (b, i, e)

let private whileParser =
    str "while" >>. ws >>. boolParser .>>. statementParser |>> While

let private loopParser =
    str "loop" >>. ws >>. pint32 .>> ws .>>. statementParser |>> Loop

statementParserRef.Value <- choice [
    runParser
    blockParser
    ifParser
    whileParser
    loopParser
]

let parse input =
    input
    |> run statementParser