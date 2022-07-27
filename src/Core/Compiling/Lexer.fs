[<RequireQualifiedAccess>]
module TGG.Core.Compiling.Lexer

open System

type Token =
// Stringing
| Quote         // '
| DoubleQuote   // "
| BackSlash     // \
| OpenParen     // (
| CloseParen    // )
| OpenBrace     // [
| CloseBrace    // ]

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

module Token =
    let getString = function
        | Quote ->         "'"
        | DoubleQuote ->   "\""
        | BackSlash ->     "\\"
        | OpenParen ->     "("
        | CloseParen ->    ")"
        | OpenBrace ->     "["
        | CloseBrace ->    "]"
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
        | WS v -> string v
        | Letter v -> string v
        | Number v -> string v
        | Char v -> string v
        | String v -> v
        | ParsedNumber v -> string v

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

let lex s =
    List.ofSeq s
    |> List.map lexer