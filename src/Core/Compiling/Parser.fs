[<RequireQualifiedAccess>]
module TGG.Core.Compiling.Parser

open TGG.Core.Compiling
open TGG.Core.Types

type Operation<'t, 'r> = Operation of Name: string * Getter: ('t -> 'r)

type Acessor = Acessor of string                                                // <<value>|'<value with spaces>'|"<value with spaces and escaping>">

type ActionAcessor = ActionAcessor of Acessor                                   // action <acessor>
type AutomationAcessor = AutomationAcessor of Acessor                           // automation <acessor>
type InventoryAcessor = InventoryAcessor of Acessor                             // inventory <acessor>
type ItemAcessor = ItemAcessor of Acessor                                       // item <acessor>

type ItemsAcessor =
| SingleItem of ItemAcessor                                                     // <item acessor>
| BlockOfItems of ItemAcessor list                                              // [ <item acessors> ]

type GenRunner<'t> = GenRunner of 't * Ignores: ItemsAcessor option             // <'t acessor> ignores <itemsAcessor>

type Runner =                                                                   // run <gen runner>
| ActionRunner of GenRunner<ActionAcessor>             
| AutomationRunner of GenRunner<AutomationAcessor>     

type InfixExpr<'t, 'r> = Infix of 't * Operation<'t, 't -> 'r> * 't
type PrefixExpr<'t, 'r> = Prefix of Operation<'t, 'r> * 't

type IntExpr =
| IntInfixExpr of InfixExpr<IntExpr, int>                                      // <expr> <op> <expr>
| IntPrefixExpr of PrefixExpr<IntExpr, int>                                    // <op><expr>
| IntParenthesis of IntExpr                                                    // (<expr>)
| IntConst of int                                                              // <number>
| InInv of InventoryAcessor                                                    // <inventory acessor>

type BoolExpr =
| BoolInfixExpr of InfixExpr<BoolExpr, bool>                                    // <expr> <op> <expr>
| Comparison of InfixExpr<IntExpr, bool>                                        // <expr> <comp> <expr>
| BoolPrefixExpr of PrefixExpr<BoolExpr, bool>                                  // <op><expr>
| BoolParenthesis of BoolExpr                                                   // (<expr>)
| BoolConst of bool                                                             // <true|false>
| CanRun of Runner                                                              // can <runner>

type Statement =
| Block of Statement list                                                       // { <statements> }
| Loop of IntExpr * Statement                                                   // loop <expr> <statement>
| While of BoolExpr * Statement                                                 // while <expr> <statement>
| If of BoolExpr * IfStatement: Statement * ElseStatement: Statement option     // if <expr> <if statement> else <else statement>
| Run of Runner                                                                 // <runner>

type ParserResult<'t> = Result<'t * Lexer.Token list, string>

let parseAcessor tokens: ParserResult<Acessor> =
    match tokens with
    | Lexer.String v::tail -> Success (Acessor v, tail)
    | _ -> Failure "Invallid Acessor"

let parseItems tokens: ParserResult<ItemsAcessor> =
    let parseItem tokens =
        match tokens with
        | Lexer.Item::tokens ->
            parseAcessor tokens
            .|>> ItemAcessor
        | _ -> Failure "No item acessor"

    let rec block tokens =
        parseItem tokens
        |=> function
        | _, [] -> Failure "Missing Closing Brace"
        | item, Lexer.CloseBrace::tokens -> Success <| ([ item ], tokens)
        | item, tokens -> block tokens .|>> (cons item)

    match tokens with
    | Lexer.OpenBrace::tokens -> block tokens .|>> BlockOfItems
    | tokens -> parseItem tokens .|>> SingleItem

let intInfixOperations =
    [ ("Plus", (+))
      ("Minus", (-))
      ("Divide", (/))
      ("Times", (*))
      ("Modulo", (%)) ]
    |> List.map (Operation)

let intPrefixOperations =
    [ ("Negative", (-)0)
      ("Positive", (+)0) ]
    |> List.map Operation


let parseIntExpr tokens =
    Success <| (IntConst 0, tokens)

let boolInfixOperations =
    [ ("An", (&&))
      ("Or", (||))
      ("Eq", (=))
      ("NE", (<>)) ]
    |> List.map (Operation)

let boolPrefixOperations =
    [ ("Not", not) ]
    |> List.map Operation

let comparisonOperations: Operation<int, int -> bool> list =
    [ ("GT", (>))
      ("GE", (>=))
      ("LT", (<))
      ("LE", (<=))
      ("Eq", (=))
      ("NE", (<>)) ]
    |> List.map Operation

let parseBoolExpr tokens =
    Success <| (BoolConst false, tokens)

let parseRun tokens: ParserResult<Acessor * ItemsAcessor option> =
    parseAcessor tokens
    |=> fun (acessor, tokens) ->
    match tokens with
    | Lexer.Ignores::tokens -> parseItems tokens .|>> Some
    | tokens -> Success (None, tokens)
    .|>> (tuple acessor)

let parseStatement tokens: ParserResult<Statement> =

    let rec p tokens =
        match tokens with
        | [] -> Failure "Can't parse empty list"
        | Lexer.OpenBracket::tail ->
            let rec block tokens =
                p tokens
                |=> function
                | _, [] -> Failure "Missing Closing Bracket"
                | statement, Lexer.CloseBracket::tail -> Success ([ statement ], tail)
                | statement, tokens -> block tokens .|>> (cons statement)
            block tail .|>> Block
        
        | Lexer.Loop::tokens ->
            parseIntExpr tokens
            |=> fun (expr, tokens) -> 
            p tokens .|>> (tuple expr >> Loop)

        | Lexer.While::tokens ->
            parseBoolExpr tokens
            |=> fun (expr, tokens) ->
            p tokens .|>> (tuple expr >> While)

        | Lexer.If::tokens ->
            parseBoolExpr tokens
            |=> fun (expr, tokens) ->
            p tokens
            |=> fun (ifStatement, tokens) ->
            match tokens with
            | Lexer.Else::tokens -> p tokens .|>> (If << triple expr ifStatement << Some)
            | tokens -> Success(If (expr, ifStatement, None), tokens)
    
        | Lexer.Run::Lexer.Action::tokens -> parseRun tokens .|>> (Run << ActionRunner << GenRunner << map1 ActionAcessor)
        | Lexer.Run::Lexer.Automation::tokens -> parseRun tokens .|>> (Run << AutomationRunner << GenRunner << map1 AutomationAcessor)

        | t::_ -> Failure <| sprintf "Invallid token '%O'" t

    p tokens

// let private parser tokens =
//     Success tokens

let parse input =
    input
    |> Lexer.lex
    // |=> parser