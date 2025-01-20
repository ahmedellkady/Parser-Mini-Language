open System
open System.Collections.Generic

// Custom Types for Abstract Syntax Tree (AST)
type Expr =
    | Number of int
    | Variable of string
    | Add of Expr * Expr
    | Subtract of Expr * Expr
    | Multiply of Expr * Expr
    | Divide of Expr * Expr
    | Comparison of string * Expr * Expr

type Statement =
    | Assignment of string * Expr
    | IfStatement of Expr * Statement list
    | WhileLoop of Expr * Statement list
    | Block of Statement list

// Custom Error Type
type ParserError =
    | SyntaxError of string
    | RuntimeError of string

// Symbol Table to store variables (using a mutable Dictionary)
type SymbolTable() =
    let mutable table = Dictionary<string, int>()

    member this.Add(name, value) = table.[name] <- value
    member this.TryFind(name) = if table.ContainsKey(name) then Some table.[name] else None
    member this.GetTable() = table

// Tokenizer Module
module Tokenizer =
    type Token =
        | NumberToken of int
        | VariableToken of string
        | OperatorToken of string
        | AssignToken
        | IfToken
        | ThenToken
        | WhileToken
        | ComparisonToken of string
        | OpenParenToken
        | CloseParenToken
        | SemicolonToken

    let isDigit c = c >= '0' && c <= '9'
    let isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    let isWhitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

    let parseWhile predicate (input: string) index =
        let mutable acc = ""
        let mutable idx = index
        while idx < input.Length && predicate input.[idx] do
            acc <- acc + string input.[idx]
            idx <- idx + 1
        (acc, idx)

    let parseNumber input index =
        let (numStr, newIndex) = parseWhile isDigit input index
        (int numStr, newIndex)

    let parseIdentifier input index =
        let (id, newIndex) = parseWhile (fun c -> isLetter c || isDigit c) input index
        (id, newIndex)

    let tokenize (input: string) =
        let mutable tokens = []
        let mutable index = 0
        while index < input.Length do
            match input.[index] with
            | ';' -> tokens <- SemicolonToken :: tokens; index <- index + 1
            | c when isWhitespace c -> index <- index + 1
            | c when isDigit c ->
                let (num, nextIndex) = parseNumber input index
                tokens <- NumberToken num :: tokens
                index <- nextIndex
            | c when isLetter c ->
                let (identifier, nextIndex) = parseIdentifier input index
                let token =
                    match identifier with
                    | "if" -> IfToken
                    | "then" -> ThenToken
                    | "while" -> WhileToken
                    | _ -> VariableToken identifier
                tokens <- token :: tokens
                index <- nextIndex
            | '+' -> tokens <- OperatorToken "+" :: tokens; index <- index + 1
            | '-' -> tokens <- OperatorToken "-" :: tokens; index <- index + 1
            | '*' -> tokens <- OperatorToken "*" :: tokens; index <- index + 1
            | '/' -> tokens <- OperatorToken "/" :: tokens; index <- index + 1
            | '=' -> tokens <- AssignToken :: tokens; index <- index + 1
            | '(' -> tokens <- OpenParenToken :: tokens; index <- index + 1
            | ')' -> tokens <- CloseParenToken :: tokens; index <- index + 1
            | '>' -> tokens <- ComparisonToken ">" :: tokens; index <- index + 1
            | '<' -> tokens <- ComparisonToken "<" :: tokens; index <- index + 1
            | _ -> failwith ("Unexpected character: " + string input.[index] + "")
        List.rev tokens

// Parser Module
module Parser =
    open Tokenizer

    let rec parseExpression tokens (symbolTable: SymbolTable) =
        let rec parsePrimaryExpression tokens =
            match tokens with
            | NumberToken n :: rest -> (Number n, rest)
            | VariableToken v :: rest -> (Variable v, rest)
            | OpenParenToken :: rest ->
                let (expr, remainingTokens) = parseExpression rest symbolTable
                match remainingTokens with
                | CloseParenToken :: finalRest -> (expr, finalRest)
                | _ -> failwith "Expected closing parenthesis"
            | _ -> failwith "Unexpected token in expression"

        let rec parseComparisonExpression tokens =
            let (leftExpr, remainingTokens) = parseAdditiveExpression tokens
            match remainingTokens with
            | ComparisonToken op :: rest ->
                let (rightExpr, finalTokens) = parseAdditiveExpression rest
                (Comparison(op, leftExpr, rightExpr), finalTokens)
            | _ -> (leftExpr, remainingTokens)

        and parseMultiplicativeExpression tokens =
            let (leftExpr, remainingTokens) = parsePrimaryExpression tokens
            match remainingTokens with
            | OperatorToken "*" :: rest ->
                let (rightExpr, finalTokens) = parseMultiplicativeExpression rest
                (Multiply(leftExpr, rightExpr), finalTokens)
            | OperatorToken "/" :: rest ->
                let (rightExpr, finalTokens) = parseMultiplicativeExpression rest
                (Divide(leftExpr, rightExpr), finalTokens)
            | _ -> (leftExpr, remainingTokens)

        and parseAdditiveExpression tokens =
            let (leftExpr, remainingTokens) = parseMultiplicativeExpression tokens
            match remainingTokens with
            | OperatorToken "+" :: rest ->
                let (rightExpr, finalTokens) = parseAdditiveExpression rest
                (Add(leftExpr, rightExpr), finalTokens)
            | OperatorToken "-" :: rest ->
                let (rightExpr, finalTokens) = parseAdditiveExpression rest
                (Subtract(leftExpr, rightExpr), finalTokens)
            | _ -> (leftExpr, remainingTokens)

        parseComparisonExpression tokens

    let rec parseStatement tokens (symbolTable: SymbolTable) =
        match tokens with
        | VariableToken v :: AssignToken :: rest ->
            let (expr, remainingTokens) = parseExpression rest symbolTable
            (Assignment(v, expr), remainingTokens)
        | IfToken :: rest ->
            let (conditionExpr, afterConditionTokens) = parseExpression rest symbolTable
            match afterConditionTokens with
            | ThenToken :: remainingTokens ->
                let (block, finalTokens) = parseBlock remainingTokens symbolTable
                (IfStatement(conditionExpr, block), finalTokens)
            | _ -> failwith "Expected 'then' after condition"
        | WhileToken :: rest ->
            let (conditionExpr, afterConditionTokens) = parseExpression rest symbolTable
            match afterConditionTokens with
            | ThenToken :: remainingTokens ->
                let (block, finalTokens) = parseBlock remainingTokens symbolTable
                (WhileLoop(conditionExpr, block), finalTokens)
            | _ -> failwith "Expected 'then' after while condition"
        | _ -> failwith "Unexpected token in statement"

    and parseBlock tokens (symbolTable: SymbolTable) =
        let mutable statements = []
        let mutable currentTokens = tokens
        while not (List.isEmpty currentTokens) do
            match currentTokens with
            | SemicolonToken :: rest -> currentTokens <- rest
            | _ ->
                let (statement, remainingTokens) = parseStatement currentTokens symbolTable
                statements <- statement :: statements
                currentTokens <- remainingTokens
        (List.rev statements, [])

// Interpreter Module
module Interpreter =
    let rec evaluateExpression expr (symbolTable: SymbolTable) =
        match expr with
        | Number n -> n
        | Variable v ->
            match symbolTable.TryFind(v) with
            | Some value -> value
            | None -> failwith ("Undefined variable: " + v)
        | Add(left, right) -> evaluateExpression left symbolTable + evaluateExpression right symbolTable
        | Subtract(left, right) -> evaluateExpression left symbolTable - evaluateExpression right symbolTable
        | Multiply(left, right) -> evaluateExpression left symbolTable * evaluateExpression right symbolTable
        | Divide(left, right) -> evaluateExpression left symbolTable / evaluateExpression right symbolTable
        | Comparison(op, left, right) ->
            let leftVal = evaluateExpression left symbolTable
            let rightVal = evaluateExpression right symbolTable
            match op with
            | ">" -> if leftVal > rightVal then 1 else 0
            | "<" -> if leftVal < rightVal then 1 else 0
            | _ -> failwith "Unsupported comparison operator"

    let rec executeStatement statement (symbolTable: SymbolTable) =
        match statement with
        | Assignment(varName, expr) ->
            let value = evaluateExpression expr symbolTable
            symbolTable.Add(varName, value)
            symbolTable
        | IfStatement(condition, block) ->
            let conditionResult = evaluateExpression condition symbolTable
            if conditionResult <> 0 then
                executeBlock block symbolTable
            else
                symbolTable
        | WhileLoop(condition, block) ->
            let mutable conditionResult = evaluateExpression condition symbolTable
            while conditionResult <> 0 do
                executeBlock block symbolTable |> ignore
                conditionResult <- evaluateExpression condition symbolTable
            symbolTable
        | Block statements ->
            executeBlock statements symbolTable

    and executeBlock statements (symbolTable: SymbolTable) =
        for statement in statements do
            executeStatement statement symbolTable |> ignore
        symbolTable

// Main Program
let runMiniLanguage input =
    try
        printfn "\n------------------------------------------------------------------------------------------------\n"
        printfn "Input: %s" input

        let tokens = Tokenizer.tokenize input
        let symbolTable = SymbolTable()

        printfn "Tokens: %A" tokens

        printfn "\n--- (AST) ---"
        let (parsedStatements, _) = Parser.parseBlock tokens symbolTable
        for statement in parsedStatements do
            printfn "%A" statement

        printfn "\n--- Program Execution ---"
        let finalSymbolTable = Interpreter.executeBlock parsedStatements symbolTable
        printfn "Final symbol table: %A" (finalSymbolTable.GetTable())
    with
    | ex -> printfn "Error: %s" ex.Message

// Test Cases
runMiniLanguage "x = 5 + 3"
runMiniLanguage "x = 5 $ 3"
runMiniLanguage "x = 5 +"
runMiniLanguage "x = (5 + 3) * 2"
runMiniLanguage "x = (5 + 3 * 2"
runMiniLanguage "x = (5 * 3) + (10 + 2)"
runMiniLanguage "x = 10 / 0"
runMiniLanguage "x = z + 2"
runMiniLanguage "x = 5 + 3; y = x * 2"
runMiniLanguage "x = 5 + 3; if x > 0 then x = x - 1"
runMiniLanguage "x = 5 + 3; y = x * 2; if y > 10 then y = y / 2"
runMiniLanguage "if x > 0 x = 2"
runMiniLanguage "ef x > 0 then x = 0"
runMiniLanguage "x = 0; while x < 5 then x = x + 1"