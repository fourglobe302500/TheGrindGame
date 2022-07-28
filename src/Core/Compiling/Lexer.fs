[<RequireQualifiedAccess>]
module TGG.Core.Compiling.Lexer

open System
open TGG.Core.Types

type Token =
// Stringing
| Quote         // '
| DoubleQuote   // "
| BackSlash     // \
| OpenParen     // (
| CloseParen    // )
| OpenBrace     // [
| CloseBrace    // ]
| OpenBracket   // {
| CloseBracket  // }

// Operators
| Plus          // +
| Minus         // -
| Star          // *
| Slash         // /
| Module        // %
| And           // &
| Pipe          // |
| Not           // !
| Equal         // =
| Less          // <
| More          // >

// Special
| WS of char

// Characters
| Letter of char
| Number of char
| Char of char

// Accumulated string
| String of string
| ParsedNumber of int

// Keywords
| Loop          // loop
| While         // while
| If            // if
| Else          // else
| Run           // run
| Ignores       // ignores
| Can           // can
| True          // true
| False         // false
| Action        // action
| Inventory     // inventory
| Item          // item
| Automation    // automation

[<RequireQualifiedAccess>]
module Token =
    let getString = function
        | Quote ->         "'"
        | DoubleQuote ->   "\""
        | BackSlash ->     "\\"
        | OpenParen ->     "("
        | CloseParen ->    ")"
        | OpenBrace ->     "["
        | CloseBrace ->    "]"
        | OpenBracket ->   "{"
        | CloseBracket ->  "}"
        | Plus ->          "+"
        | Minus ->         "-"
        | Star ->          "*"
        | Slash ->         "/"
        | Module ->        "%"
        | And ->           "&"
        | Pipe ->          "|"
        | Not ->           "!"
        | Equal ->         "="
        | Less ->          "<"
        | More ->          ">"
        | Loop ->          "loop"
        | While ->         "while"
        | If ->            "if"
        | Else ->          "else"
        | Run ->           "run"
        | Ignores ->       "ignores"
        | Can ->           "can"
        | True ->          "true"
        | False ->         "false"
        | Action ->        "action"
        | Inventory ->     "inventory"
        | Item ->          "item"
        | Automation ->    "automation"
        | WS v -> string v
        | Letter v -> string v
        | Number v -> string v
        | Char v -> string v
        | String v -> v
        | ParsedNumber v -> string v
    
    let (|IsKeyword|_|) text =
        match text with
        | "while" -> Some While         
        | "if" -> Some If            
        | "else" -> Some Else          
        | "run" -> Some Run           
        | "ignores" -> Some Ignores       
        | "can" -> Some Can           
        | "true" -> Some True          
        | "false" -> Some False         
        | "action" -> Some Action        
        | "inventory" -> Some Inventory     
        | "item" -> Some Item          
        | "automation" -> Some Automation    
        | _ -> None

let lexer c =
    match c with
    | '\'' -> Quote
    | '"' -> DoubleQuote
    | '\\' -> BackSlash
    | '(' -> OpenParen 
    | ')' -> CloseParen
    | '[' -> OpenBrace 
    | ']' -> CloseBrace
    | '+' -> Plus  
    | '-' -> Minus 
    | '*' -> Star  
    | '/' -> Slash 
    | '%' -> Module
    | '&' -> And   
    | '|' -> Pipe  
    | '!' -> Not   
    | '=' -> Equal 
    | '<' -> Less  
    | '>' -> More  
    | ws when Char.IsWhiteSpace ws -> WS ws
    | l when Char.IsLetter l -> Letter l
    | n when Char.IsDigit n -> Number n
    | i -> Char i

type private State =
| Blank
| Normal of string
| Number of string
| OnQuote of string * IsDouble: bool * Escaping: bool

let batch tokens =
    let head v2 v1 = v1::v2

    let rev f v2 v1 = f v1 v2

    let fold (result, state) token =
        let Single = OnQuote("", false, false)
        let Double = OnQuote("", true, false)

        match result with
        | Failure f -> Failure f, state
        | Success result ->
        match state, token with
        // pass through
        | s, ParsedNumber _ | s, String _ | s, Loop
        | s, While | s, If | s, Else
        | s, Run | s, Ignores | s, Can
        | s, True | s, False | s, Action
        | s, Inventory | s, Item | s, Automation
            -> (Success result), s

        // operators with blank
        | Blank, (BackSlash as token) 
        | Blank, (OpenParen as token) 
        | Blank, (CloseParen as token) 
        | Blank, (OpenBrace as token) 
        | Blank, (CloseBrace as token) 
        | Blank, (OpenBracket as token)
        | Blank, (CloseBracket as token)
        | Blank, (Plus as token) 
        | Blank, (Minus as token) 
        | Blank, (Star as token) 
        | Blank, (Slash as token) 
        | Blank, (Module as token) 
        | Blank, (And as token) 
        | Blank, (Pipe as token) 
        | Blank, (Not as token) 
        | Blank, (Equal as token) 
        | Blank, (Less as token) 
        | Blank, (More as token) 
            -> Success <| token::result, Blank

        // consume ws
        | Blank, WS _ -> Success result, Blank

        // new normal from blank
        | Blank, Char c
        | Blank, Letter c
            -> Success<| result, Normal <| string c

        // new number from blank
        | Blank, Token.Number n -> Success <| result, Number <| string n

        // new quoted from blank
        | Blank, Quote -> Success <| result, Single
        | Blank, DoubleQuote -> Success <| result, Double

        // extending normal
        | Normal s, Letter c
        | Normal s, Char c
        | Normal s, Token.Number c
            -> Success result, Normal <| s + string c

        // end normal with ws
        | Normal s, WS  _
            -> Success <| String s::result, Blank

        // end normal with new tokens
        | Normal s, (OpenBrace as t)
        | Normal s, (OpenParen as t)
        | Normal s, (CloseBrace as t)
        | Normal s, (CloseParen as t)
            -> Success <| String s::t::result, Blank

        // end normal with new quoted
        | Normal s, Quote 
            -> Success <| String s::result, Single
        | Normal s, DoubleQuote
            -> Success <| String s::result, Double

        // end normal with operator
        | Normal s, (BackSlash as token) 
        | Normal s, (OpenParen as token) 
        | Normal s, (CloseParen as token) 
        | Normal s, (OpenBrace as token) 
        | Normal s, (CloseBrace as token) 
        | Normal s, (OpenBracket as token) 
        | Normal s, (CloseBracket as token) 
        | Normal s, (Plus as token) 
        | Normal s, (Minus as token) 
        | Normal s, (Star as token) 
        | Normal s, (Slash as token) 
        | Normal s, (Module as token) 
        | Normal s, (And as token) 
        | Normal s, (Pipe as token) 
        | Normal s, (Not as token) 
        | Normal s, (Equal as token) 
        | Normal s, (Less as token) 
        | Normal s, (More as token) 
            -> Success <| String s::token::result, Blank

        // extend quoted without escaping
        | OnQuote(s, double, false), (Letter _ as t)
        | OnQuote(s, double, false), (Char _ as t)
        | OnQuote(s, double, _), (Token.Number _ as t)
        | OnQuote(s, double, _), (WS _ as t)
        | OnQuote(s, double, true), (BackSlash as t) 
        | OnQuote(s, double, _), (OpenParen as t) 
        | OnQuote(s, double, _), (CloseParen as t) 
        | OnQuote(s, double, _), (OpenBrace as t) 
        | OnQuote(s, double, _), (CloseBrace as t) 
        | OnQuote(s, double, _), (OpenBracket as t) 
        | OnQuote(s, double, _), (CloseBracket as t) 
        | OnQuote(s, double, _), (Plus as t) 
        | OnQuote(s, double, _), (Minus as t) 
        | OnQuote(s, double, _), (Star as t) 
        | OnQuote(s, double, _), (Slash as t) 
        | OnQuote(s, double, _), (Module as t) 
        | OnQuote(s, double, _), (And as t) 
        | OnQuote(s, double, _), (Pipe as t) 
        | OnQuote(s, double, _), (Not as t) 
        | OnQuote(s, double, _), (Equal as t) 
        | OnQuote(s, double, _), (Less as t) 
        | OnQuote(s, double, _), (More as t)
            -> (Success result, OnQuote(s+Token.getString t, double, false))
        
        // extend quoted with escaping
        | OnQuote(s, double, true), Letter c
        | OnQuote(s, double, true), Char c
            -> 
            let escape = function 
            | 'n' -> "\n"
            | 't' -> "\t"
            | 'r' -> "\r"
            | c -> string c         
            Success result, OnQuote(s+escape c, double, false)

        // adding quotes on single quoted
        | OnQuote(s, false, true), (Quote as t)
            -> (Success result, OnQuote(s+Token.getString t, true, false))
        | OnQuote(s, false, _), (DoubleQuote as t)
            -> (Success result, OnQuote(s+Token.getString t, true, false))
        
        // adding quotes on Double quoted
        | OnQuote(s, true, _), (Quote as t)
            -> (Success result, OnQuote(s+Token.getString t, true, false))
        | OnQuote(s, true, true), (DoubleQuote as t)
            -> (Success result, OnQuote(s+Token.getString t, true, false))

        // ending quoted
        | OnQuote(s, false, false), Quote 
        | OnQuote(s, true, false), DoubleQuote 
            -> Success <| String s::result, Blank

        // escaping
        | OnQuote(s, double, false), (BackSlash)
            -> Success result, OnQuote(s, double, true)

        // extending number
        | Number n, Token.Number c -> Success result, Number <| n + string c

        // ending number with string
        | Number n, Char c
        | Number n, Letter c
            -> Success << head result << ParsedNumber <| int n, Normal <| string c

        | Number n, Quote -> Success << head result << ParsedNumber <| int n, Single      
        | Number n, DoubleQuote -> Success << head result << ParsedNumber <| int n, Double
        
        // ending number with whiteSpace
        | Number n, WS _ -> Success << head result << ParsedNumber <| int n, Blank

        // ending number with operation
        | Number n, (OpenParen as t)
        | Number n, (CloseParen as t)
        | Number n, (OpenBrace as t)
        | Number n, (CloseBrace as t)
        | Number n, (OpenBracket as t)
        | Number n, (CloseBracket as t)
        | Number n, (BackSlash as t) 
        | Number n, (Plus as t) 
        | Number n, (Minus as t) 
        | Number n, (Star as t) 
        | Number n, (Slash as t) 
        | Number n, (Module as t) 
        | Number n, (And as t) 
        | Number n, (Pipe as t) 
        | Number n, (Not as t) 
        | Number n, (Equal as t) 
        | Number n, (Less as t) 
        | Number n, (More as t) 
            -> Success << head (t::result) << ParsedNumber <| int n, Blank

    tokens
    |> List.fold fold (Success [], Blank)
    |> function 
    | Success r,  Blank -> Success <| List.rev r
    | Success r, Number s -> Success << List.rev << head r << ParsedNumber <| int s
    | Success r, Normal s -> Success << List.rev <| String s::r
    | Success _, OnQuote _ -> Failure <| "Unfinished quoted string"
    | Failure f, _ -> Failure f

let bindKeywords tokens =
    let toKey token = 
        match token with
        | String (Token.IsKeyword key) ->
            key
        | token -> token
    tokens
    |> List.map toKey

let lex (s: string) =
    List.ofSeq s
    |> List.map lexer
    |> batch
    |>> bindKeywords