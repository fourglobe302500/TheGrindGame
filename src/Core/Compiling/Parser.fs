[<RequireQualifiedAccess>]
module TGG.Core.Compiling.Parser

open TGG.Core.Types
open TGG.Core.Compiling

type Operation<'t, 'r> = Operation of Name: string * Getter: ('t -> 'r)

type Accessor = Accessor of string                                              // <<value>|'<value with spaces>'|"<value with spaces and escaping>">

type ActionAccessor = ActionAccessor of Accessor                                // action <accessor>
type AutomationAccessor = AutomationAccessor of Accessor                        // automation <accessor>
type InventoryAccessor = InventoryAccessor of Accessor                          // inventory <accessor>
type ItemAccessor = ItemAccessor of Accessor                                    // item <accessor>

type GenRunner<'t> = GenRunner of 't * Ignores: ItemAccessor list               // <'t accessor> ignores <<item>|[ <items> ]>

type Runner =                                                                   // run <gen runner>
| ActionRunner of GenRunner<ActionAccessor>             
| AutomationRunner of GenRunner<AutomationAccessor>     

type InfixExpr<'t, 'r> = Infix of 't * Operation<'t * 't, 'r> * 't
type PrefixExpr<'t, 'r> = Prefix of Operation<'t, 'r> * 't

type IntExpr =
| InfixExpr of InfixExpr<IntExpr, int>                                          // <expr> <op> <expr>
| PrefixExpr of PrefixExpr<IntExpr, int>                                        // <op><expr>
| Parenthesis of IntExpr                                                        // (<expr>)
| Const of int                                                                  // <number>
| InInv of InventoryAccessor                                                    // <inventory accessor>

type BoolExpr =
| InfixExpr of InfixExpr<BoolExpr, bool>                                        // <expr> <op> <expr>
| Comparison of InfixExpr<IntExpr, bool>                                        // <expr> <comp> <expr>
| PrefixExpr of PrefixExpr<BoolExpr, bool>                                      // <op><expr>
| Parenthesis of BoolExpr                                                       // (<expr>)
| Const of bool                                                                 // <true|false>
| CanRun of Runner                                                              // can <runner>

type Statement =
| Block of Statement list                                                       // { <statements> }
| Loop of IntExpr * Statement                                                   // loop <expr> <statement>
| While of BoolExpr * Statement                                                 // while <expr> <statement>
| If of BoolExpr * IfStatement: Statement * ElseStatement: Statement option     // if <expr> <if statement> else <else statement>
| Run of Runner                                                                 // <runner>

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
        | Global.Failure f -> Global.Failure f, state
        | Global.Success result ->
        match state, token with
        // pass through
        | s, Lexer.ParsedNumber _ 
        | s, Lexer.String _ 
            -> (Global.Success result), s

        // operators with blank
        | Blank, (Lexer.BackSlash as token) 
        | Blank, (Lexer.OpenParen as token) 
        | Blank, (Lexer.CloseParen as token) 
        | Blank, (Lexer.OpenBrace as token) 
        | Blank, (Lexer.CloseBrace as token) 
        | Blank, (Lexer.Plus as token) 
        | Blank, (Lexer.Minus as token) 
        | Blank, (Lexer.Star as token) 
        | Blank, (Lexer.Slash as token) 
        | Blank, (Lexer.Module as token) 
        | Blank, (Lexer.And as token) 
        | Blank, (Lexer.Pipe as token) 
        | Blank, (Lexer.Not as token) 
        | Blank, (Lexer.Equal as token) 
        | Blank, (Lexer.Less as token) 
        | Blank, (Lexer.More as token) 
            -> Global.Success <| token::result, Blank

        // consume ws
        | Blank, Lexer.WS _ -> Global.Success result, Blank

        // new normal from blank
        | Blank, Lexer.Char c
        | Blank, Lexer.Letter c
            -> Global.Success<| result, Normal <| string c

        // new number from blank
        | Blank, Lexer.Number n -> Global.Success <| result, Number <| string n

        // new quoted from blank
        | Blank, Lexer.Quote -> Global.Success <| result, Single
        | Blank, Lexer.DoubleQuote -> Global.Success <| result, Double

        // extending normal
        | Normal s, Lexer.Letter c
        | Normal s, Lexer.Char c
        | Normal s, Lexer.Number c
            -> Global.Success result, Normal <| s + string c

        // end normal with ws
        | Normal s, Lexer.WS  _
            -> Global.Success <| Lexer.String s::result, Blank

        // end normal with new tokens
        | Normal s, (Lexer.OpenBrace as t)
        | Normal s, (Lexer.OpenParen as t)
        | Normal s, (Lexer.CloseBrace as t)
        | Normal s, (Lexer.CloseParen as t)
            -> Global.Success <| Lexer.String s::t::result, Blank

        // end normal with new quoted
        | Normal s, Lexer.Quote 
            -> Global.Success <| Lexer.String s::result, Single
        | Normal s, Lexer.DoubleQuote
            -> Global.Success <| Lexer.String s::result, Double

        // end normal with operator
        | Normal s, (Lexer.BackSlash as token) 
        | Normal s, (Lexer.OpenParen as token) 
        | Normal s, (Lexer.CloseParen as token) 
        | Normal s, (Lexer.OpenBrace as token) 
        | Normal s, (Lexer.CloseBrace as token) 
        | Normal s, (Lexer.Plus as token) 
        | Normal s, (Lexer.Minus as token) 
        | Normal s, (Lexer.Star as token) 
        | Normal s, (Lexer.Slash as token) 
        | Normal s, (Lexer.Module as token) 
        | Normal s, (Lexer.And as token) 
        | Normal s, (Lexer.Pipe as token) 
        | Normal s, (Lexer.Not as token) 
        | Normal s, (Lexer.Equal as token) 
        | Normal s, (Lexer.Less as token) 
        | Normal s, (Lexer.More as token) 
            -> Global.Success <| Lexer.String s::token::result, Blank

        // extend quoted without escaping
        | OnQuote(s, double, false), (Lexer.Letter _ as t)
        | OnQuote(s, double, false), (Lexer.Char _ as t)
        | OnQuote(s, double, _), (Lexer.Number _ as t)
        | OnQuote(s, double, _), (Lexer.WS _ as t)
        | OnQuote(s, double, _), (Lexer.OpenBrace as t)
        | OnQuote(s, double, _), (Lexer.OpenParen as t)
        | OnQuote(s, double, _), (Lexer.CloseBrace as t)
        | OnQuote(s, double, _), (Lexer.CloseParen as t)
        | OnQuote(s, double, true), (Lexer.BackSlash as t) 
        | OnQuote(s, double, _), (Lexer.OpenParen as t) 
        | OnQuote(s, double, _), (Lexer.CloseParen as t) 
        | OnQuote(s, double, _), (Lexer.OpenBrace as t) 
        | OnQuote(s, double, _), (Lexer.CloseBrace as t) 
        | OnQuote(s, double, _), (Lexer.Plus as t) 
        | OnQuote(s, double, _), (Lexer.Minus as t) 
        | OnQuote(s, double, _), (Lexer.Star as t) 
        | OnQuote(s, double, _), (Lexer.Slash as t) 
        | OnQuote(s, double, _), (Lexer.Module as t) 
        | OnQuote(s, double, _), (Lexer.And as t) 
        | OnQuote(s, double, _), (Lexer.Pipe as t) 
        | OnQuote(s, double, _), (Lexer.Not as t) 
        | OnQuote(s, double, _), (Lexer.Equal as t) 
        | OnQuote(s, double, _), (Lexer.Less as t) 
        | OnQuote(s, double, _), (Lexer.More as t)
            -> (Global.Success result, OnQuote(s+Lexer.Token.getString t, double, false))
        
        // extend quoted with escaping
        | OnQuote(s, double, true), Lexer.Letter c
        | OnQuote(s, double, true), Lexer.Char c
            -> 
            let escape = function 
            | 'n' -> "\n"
            | 't' -> "\t"
            | 'r' -> "\r"
            | c -> string c         
            Global.Success result, OnQuote(s+escape c, double, false)

        // adding quotes on single quoted
        | OnQuote(s, false, true), (Lexer.Quote as t)
            -> (Global.Success result, OnQuote(s+Lexer.Token.getString t, true, false))
        | OnQuote(s, false, _), (Lexer.DoubleQuote as t)
            -> (Global.Success result, OnQuote(s+Lexer.Token.getString t, true, false))
        
        // adding quotes on Double quoted
        | OnQuote(s, true, _), (Lexer.Quote as t)
            -> (Global.Success result, OnQuote(s+Lexer.Token.getString t, true, false))
        | OnQuote(s, true, true), (Lexer.DoubleQuote as t)
            -> (Global.Success result, OnQuote(s+Lexer.Token.getString t, true, false))

        // ending quoted
        | OnQuote(s, false, false), Lexer.Quote 
        | OnQuote(s, true, false), Lexer.DoubleQuote 
            -> Global.Success <| Lexer.String s::result, Blank

        // escaping
        | OnQuote(s, double, false), (Lexer.BackSlash)
            -> Global.Success result, OnQuote(s, double, true)

        // extending number
        | Number n, Lexer.Number c -> Global.Success result, Number <| n + string c

        // ending number with string
        | Number n, Lexer.Char c
        | Number n, Lexer.Letter c
            -> Global.Success << head result << Lexer.ParsedNumber <| int n, Normal <| string c

        | Number n, Lexer.Quote -> Global.Success << head result << Lexer.ParsedNumber <| int n, Single      
        | Number n, Lexer.DoubleQuote -> Global.Success << head result << Lexer.ParsedNumber <| int n, Double
        
        // ending number with whiteSpace
        | Number n, Lexer.WS _ -> Global.Success << head result << Lexer.ParsedNumber <| int n, Blank

        // ending number with operation
        | Number n, (Lexer.OpenBrace as t)
        | Number n, (Lexer.OpenParen as t)
        | Number n, (Lexer.CloseBrace as t)
        | Number n, (Lexer.CloseParen as t)
        | Number n, (Lexer.BackSlash as t) 
        | Number n, (Lexer.OpenParen as t) 
        | Number n, (Lexer.CloseParen as t) 
        | Number n, (Lexer.OpenBrace as t) 
        | Number n, (Lexer.CloseBrace as t) 
        | Number n, (Lexer.Plus as t) 
        | Number n, (Lexer.Minus as t) 
        | Number n, (Lexer.Star as t) 
        | Number n, (Lexer.Slash as t) 
        | Number n, (Lexer.Module as t) 
        | Number n, (Lexer.And as t) 
        | Number n, (Lexer.Pipe as t) 
        | Number n, (Lexer.Not as t) 
        | Number n, (Lexer.Equal as t) 
        | Number n, (Lexer.Less as t) 
        | Number n, (Lexer.More as t) 
            -> Global.Success << head (t::result) << Lexer.ParsedNumber <| int n, Blank

    tokens
    |> List.fold fold (Global.Success [], Blank)
    |> function 
    | Global.Success r,  Blank -> Global.Success <| List.rev r
    | Global.Success r, Number s -> Global.Success << List.rev << head r << Lexer.ParsedNumber <| int s
    | Global.Success r, Normal s -> Global.Success << List.rev <| Lexer.String s::r
    | Global.Success _, OnQuote _ -> Global.Failure <| "Unfinished quoted string"
    | Global.Failure f, _ -> Global.Failure f

let parse input =
    input
    |> Lexer.lex
    |> batch