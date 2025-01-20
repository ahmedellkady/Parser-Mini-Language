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
    | IfStatement of Expr * Statement list * Statement list option
    | WhileLoop of Expr * Statement list
    | Block of Statement list

// Custom Error Type
type ParserError = 
    | SyntaxError of string
    | RuntimeError of string

// Symbol Table to store variables
type SymbolTable = Map<string, int>

module Tokenizer = begin
    // Custom Token Type
    type Token = 
        | NumberToken of int
        | VariableToken of string
        | OperatorToken of string
        | AssignToken
        | IfToken
        | ThenToken
        | ElseToken
        | WhileToken
        | ComparisonToken of string
        | OpenParenToken
        | CloseParenToken
        | SemicolonToken

    // Helper functions to not use built-in functions
    let isDigit c = c >= '0' && c <= '9'
    let isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    let isWhitespace c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
    
    // Higher-order function for parsing a sequence of characters
    let parseWhile (predicate: char -> bool) (input: string) (index: int) =
        let rec loop acc idx =
            if idx < input.Length && predicate input.[idx] then
                loop (acc + string input.[idx]) (idx + 1)
            else
                (acc, idx)
        loop "" index

    // Function to parse numbers
    let parseNumber (input: string) (index: int) =
        let (numStr, newIndex) = parseWhile isDigit input index
        (int numStr, newIndex)

    // Function to parse identifiers (variable names or keywords)
    let parseIdentifier (input: string) (index: int) =
        let (id, newIndex) = parseWhile (fun c -> isLetter c || isDigit c) input index
        (id, newIndex)

    // Function to handle tokenization logic
    let tokenize (input: string) =
        let rec tokenizeRec tokens index =
            if index >= input.Length then 
                List.rev tokens
            else
                let nextToken, newIndex = 
                    match input.[index] with
                    | ';' -> (SemicolonToken :: tokens, index + 1)
                    | c when isWhitespace c -> (tokens, index + 1)
                    | c when isDigit c -> 
                        let (num, nextIndex) = parseNumber input index
                        (NumberToken num :: tokens, nextIndex)
                    | c when isLetter c -> 
                        let (identifier, nextIndex) = parseIdentifier input index
                        let token = 
                            match identifier with
                            | "if" -> IfToken
                            | "then" -> ThenToken
                            | "else" -> ElseToken
                            | "while" -> WhileToken
                            | _ -> VariableToken identifier
                        (token :: tokens, nextIndex)
                    | '+' -> (OperatorToken "+" :: tokens, index + 1)
                    | '-' -> (OperatorToken "-" :: tokens, index + 1)
                    | '*' -> (OperatorToken "*" :: tokens, index + 1)
                    | '/' -> (OperatorToken "/" :: tokens, index + 1)
                    | '=' -> (AssignToken :: tokens, index + 1)
                    | '(' -> (OpenParenToken :: tokens, index + 1)
                    | ')' -> (CloseParenToken :: tokens, index + 1)
                    | '>' -> (ComparisonToken ">" :: tokens, index + 1)
                    | '<' -> (ComparisonToken "<" :: tokens, index + 1)
                    | _ -> failwith ("Unexpected character: " + string input.[index] + "")
                
                tokenizeRec nextToken newIndex
        
        tokenizeRec [] 0
end


module Parser = begin
    open Tokenizer

    let rec parseExpression tokens symbolTable =
        let rec parsePrimaryExpression tokens =
            match tokens with
            | NumberToken n :: rest -> 
                (Number n, rest)
            | VariableToken v :: rest -> 
                (Variable v, rest)
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

    let rec parseStatement tokens symbolTable =
        match tokens with
        | VariableToken v :: AssignToken :: rest ->
            let (expr, remainingTokens) = parseExpression rest symbolTable
            (Assignment(v, expr), remainingTokens)
        | IfToken :: rest ->
            // Parse the condition expression
            let (conditionExpr, afterConditionTokens) = parseExpression rest symbolTable
            
            // Check for ThenToken
            match afterConditionTokens with
            | ThenToken :: remainingTokens ->
                // Parse the if block
                let (ifBlock, afterIfBlockTokens) = parseBlock remainingTokens symbolTable
                
                // Check for ElseToken
                match afterIfBlockTokens with
                | ElseToken :: elseTokens ->
                    // Parse the else block
                    let (elseBlock, finalTokens) = parseBlock elseTokens symbolTable
                    (IfStatement(conditionExpr, ifBlock, Some elseBlock), finalTokens)
                | _ ->
                    // No else block
                    (IfStatement(conditionExpr, ifBlock, None), afterIfBlockTokens)
            | _ -> failwith "Expected 'then' after condition"
        | WhileToken :: rest ->
            let (conditionExpr, afterConditionTokens) = parseExpression rest symbolTable
            match afterConditionTokens with
            | ThenToken :: remainingTokens ->
                let (block, finalTokens) = parseBlock remainingTokens symbolTable
                (WhileLoop(conditionExpr, block), finalTokens)
            | _ -> failwith "Expected 'then' after while condition"
        | _ -> failwith "Unexpected token in statement"

    and parseBlock tokens symbolTable =
        let rec parseMultipleStatements accStatements currentTokens =
            match currentTokens with
            | [] -> (List.rev accStatements, []) // No more tokens, return the accumulated statements
            | SemicolonToken :: rest -> // Skip semicolons and continue parsing
                parseMultipleStatements accStatements rest
            | _ -> // Parse the next statement
                let (statement, remainingTokens) = parseStatement currentTokens symbolTable
                match remainingTokens with
                | SemicolonToken :: finalRest -> // Expect semicolon after each statement
                    parseMultipleStatements (statement :: accStatements) finalRest
                | [] -> (List.rev (statement :: accStatements), []) // Last statement, no semicolon needed
                | ElseToken :: _ -> // Handle else block as part of the if statement
                    parseMultipleStatements (statement :: accStatements) remainingTokens
                | _ -> failwith "Expected semicolon after statement"
        
        parseMultipleStatements [] tokens
end

module Interpreter = begin
    let rec evaluateExpression expr symbolTable =
        match expr with
        | Number n -> n
        | Variable v -> 
            match Map.tryFind v symbolTable with
            | Some value -> value
            | None -> failwith ("Undefined variable: " + v + "")
        | Add(left, right) -> 
            evaluateExpression left symbolTable + evaluateExpression right symbolTable
        | Subtract(left, right) -> 
            evaluateExpression left symbolTable - evaluateExpression right symbolTable
        | Multiply(left, right) -> 
            evaluateExpression left symbolTable * evaluateExpression right symbolTable
        | Divide(left, right) -> 
            evaluateExpression left symbolTable / evaluateExpression right symbolTable
        | Comparison(op, left, right) ->
            let leftVal = evaluateExpression left symbolTable
            let rightVal = evaluateExpression right symbolTable
            match op with
            | ">" -> if leftVal > rightVal then 1 else 0
            | "<" -> if leftVal < rightVal then 1 else 0
            | _ -> failwith "Unsupported comparison operator"

    let rec executeStatement statement symbolTable =
        match statement with
        | Assignment(varName, expr) ->
            let value = evaluateExpression expr symbolTable
            Map.add varName value symbolTable
        | IfStatement(condition, ifBlock, elseBlock) ->
            let conditionResult = evaluateExpression condition symbolTable
            if conditionResult <> 0 then
                executeBlock ifBlock symbolTable
            else
                match elseBlock with
                | Some block -> executeBlock block symbolTable
                | None -> symbolTable
        | WhileLoop(condition, block) ->
            let rec loop table =
                let conditionResult = evaluateExpression condition table
                if conditionResult <> 0 then
                    let newTable = executeBlock block table
                    loop newTable
                else
                    table
            loop symbolTable
        | Block statements ->
            executeBlock statements symbolTable

    and executeBlock statements symbolTable =
        List.fold (fun table stmt -> executeStatement stmt table) symbolTable statements

    let interpret input =
        let tokens = Tokenizer.tokenize input
        let initialSymbolTable = Map.empty
        let (parsedStatement, _) = Parser.parseStatement tokens initialSymbolTable
        let finalSymbolTable = executeStatement parsedStatement initialSymbolTable
        finalSymbolTable
end

module AstPrinter = begin
    let rec printExpr indent expr =
        let spaces = String.replicate indent " "
        match expr with
        | Number n -> 
            printfn "%sNumber(%d)" spaces n
        | Variable v -> 
            printfn "%sVariable(%s)" spaces v
        | Add(left, right) -> 
            printfn "%sAdd(" spaces
            printExpr (indent + 2) left
            printExpr (indent + 2) right
            printfn "%s)" spaces
        | Subtract(left, right) -> 
            printfn "%sSubtract(" spaces
            printExpr (indent + 2) left
            printExpr (indent + 2) right
            printfn "%s)" spaces
        | Multiply(left, right) -> 
            printfn "%sMultiply(" spaces
            printExpr (indent + 2) left
            printExpr (indent + 2) right
            printfn "%s)" spaces
        | Divide(left, right) -> 
            printfn "%sDivide(" spaces
            printExpr (indent + 2) left
            printExpr (indent + 2) right
            printfn "%s)" spaces
        | Comparison(op, left, right) ->
            printfn "%sComparison(%s)" spaces op
            printExpr (indent + 2) left
            printExpr (indent + 2) right

    let rec printStatement indent statement =
        let spaces = String.replicate indent " "
        match statement with
        | Assignment(varName, expr) ->
            printfn "%sAssignment(%s)" spaces varName
            printExpr (indent + 2) expr
        | IfStatement(condition, ifBlock, elseBlock) ->
            printfn "%sIfStatement(" spaces
            printfn "%sCondition:" (spaces + "  ")
            printExpr (indent + 4) condition
            printfn "%sIf Block:" (spaces + "  ")
            List.iter (printStatement (indent + 4)) ifBlock
            match elseBlock with
            | Some block ->
                printfn "%sElse Block:" (spaces + "  ")
                List.iter (printStatement (indent + 4)) block
            | None -> ()
            printfn "%s)" spaces
        | WhileLoop(condition, block) ->
            printfn "%sWhileLoop(" spaces
            printfn "%sCondition:" (spaces + "  ")
            printExpr (indent + 4) condition
            printfn "%sBlock:" (spaces + "  ")
            List.iter (printStatement (indent + 4)) block
            printfn "%s)" spaces
        | Block statements ->
            printfn "%sBlock:" spaces
            List.iter (printStatement (indent + 2)) statements
end

let runMiniLanguage input =
    try 
        printfn "\n------------------------------------------------------------------------------------------------\n"
        printfn "Input: %s" input

        let tokens = Tokenizer.tokenize input
        let initialSymbolTable = Map.empty

        printfn "Tokens: %A" tokens
        
        printfn "\n--- (AST) ---"
        let (parsedStatements, _) = Parser.parseBlock tokens initialSymbolTable
        List.iter (AstPrinter.printStatement 0) parsedStatements
        
        printfn "\n--- Program Execution ---"
        let finalSymbolTable = List.fold (fun table stmt -> Interpreter.executeStatement stmt table) initialSymbolTable parsedStatements
        printfn "Final symbol table: %A " finalSymbolTable
    with 
    | ex -> printfn "Error: %s" ex.Message

runMiniLanguage "x = 5 + 3"
runMiniLanguage "x = 5 $ 3"
runMiniLanguage "x = 5 +"
runMiniLanguage "x = (5 + 3) * 2"
runMiniLanguage "x = (5 + 3 * 2"
runMiniLanguage "x = (5 * 3) + (10 + 2)"
runMiniLanguage "x = 10 / 0"
runMiniLanguage "x = z + 2"
runMiniLanguage "x = 5 + 3; y = x * 2;"
runMiniLanguage "x = 5 + 3 y = x * 2;"
runMiniLanguage "x = 5 + 3; if x > 0 then x = x - 1"
runMiniLanguage "x = 5 + 3; y = x * 2; if y > 10 then y = y / 2"
runMiniLanguage "x = 10; y = 20; z = 0; if y > x then z = 100"
runMiniLanguage "if x > 0 x = 2"
runMiniLanguage "ef x > 0 then x = 0"
runMiniLanguage "x = 0; while x < 5 then x = x + 1"