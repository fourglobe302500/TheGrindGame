[<RequireQualifiedAccess>]
module TGG.Core.Compiling.Parser

open TGG.Core.Compiling
open TGG.Core.Types

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

let parse input =
    input
    |> Lexer.lex