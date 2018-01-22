namespace JStar

open JStar
open JStar.Scanner
open Lexer

module Parser =

    type ReferenceType =
    | RefConstant of string
    | RefMutable of string
    | RefFunction of string

    type TableValue = 
    | ValReference of string
    | ValStatic of Scanner.Value
    | ValSubtable of TableValue list

    type TableKey =
    | TKNumeric of int
    | TKName of string

    type TableProperty = TableProperty of TableKey * TableValue

    type TableDefinition = 
    | TableProp of TableProperty
    | SubTable of TableDefinition

    /// Abstract Parser Tree
    type Parselet = 
    | LetBinding of string * Parselet
    | FunctionDef of string * Parselet * Parselet // function of name * parameters * body
    | FunctionCall of string * Parselet
    | OpUnary of Operators * Parselet
    | OpBinary of Operators * Parselet * Parselet
    | OpTernary of Parselet * Parselet * Parselet
    | If of Parselet * Parselet * Parselet option
    | Block of Parselet option list
    | BlockStop
    | Identifier of string
    | TableDef of Parselet
    | ValueToken of Scanner.Value
    | KeyValue of TableKey * Parselet
    | Group of Parselet list
    | Return of Parselet option
    | Token of Scanner.Symbols

    let checkExpect expect token =
        let errorMessage = sprintf "Unexpected token %A (%A)" token expect
        match expect with
        | Some allowed ->
            if allowed |> List.contains token then
                token
            else 
                failwith errorMessage        
        | None -> token            

    let someOrError message maybe = 
        match maybe with
        | Some value -> value
        | None -> failwith message

    let getNameValue nameToken =
        let errorMessage = sprintf "Unexpected token %A" nameToken
        match nameToken with
        | Identifier value -> value
        | _ -> failwith errorMessage         

    let getOperator opToken =
        let errorMessage = sprintf "Expected operator token, but got %A" opToken
        match opToken with
        | Token scan -> 
            match scan with
            | SymOp op -> op
            | _ -> failwith errorMessage    
        | _ -> failwith errorMessage  

    let expectToken token =
        let errorMessage = sprintf "Expected token, but got %A" token
        match token with
        | Token tok -> tok
        | _ -> failwith errorMessage

    let isSym sym token = token = sym

    let expectSym sym token =
        let errorMessage = sprintf "Expected symbol %A, but got %A" sym token
        if (isSym sym token) then
            sym
        else         
            failwith errorMessage

    let expectOperator (expect : Operators) opToken =
        let errorMessage = sprintf "Expected operator %A, but got %A" expect opToken
        let op = getOperator opToken
        if op = expect then 
            op
        else
            failwith errorMessage        

    let expectEqual = expectOperator OCompareEqual
    let expectAssign = expectOperator OAssign

    let extractParamNames (tokens : Parselet list) =
        tokens
        |> List.map getNameValue

    let filterCommas (tokens : Parselet list) = 
        tokens |> List.filter( fun tk -> match tk with | Token(SymPunct(PComma)) -> false | _ -> true )

    let takeA (a, _) = a

    let infixOp op left right = OpBinary(op, left, right)
    let prefixOp op token = OpUnary(op, token)
    let ternaryOp op tokenA tokenB = OpTernary(op, tokenA, tokenB)
    let getParserOperation op token =
        match op with
        | OAssign -> (infixOp op token, 80)
        | OCustom("*") -> (infixOp op token, 60)
        | OCustom("/") -> (infixOp op token, 60)
        | OCompareEqual -> (infixOp op token, 50)
        | OCompareUnequal -> (infixOp op token, 50)
        | OCustom("-") -> (infixOp op token, 50)
        | OCustom("+") -> (infixOp op token, 50)
        | OCustom(_) -> (infixOp op token, 40)
        | _ -> failwith "Broken Op"

    let rec parse isDefinition isTable scope (tokens : Scanner.Symbols list) =
        let mutable localScope = scope
        let getNext isDef isTab tokenList =
            let (result, rest) = parse isDef isTab localScope tokenList
            (result |> someOrError "Expected name", rest)
        let getNextSym (tail: Scanner.Symbols list) = (tail.Head, tail.Tail)
        let getNextn (tail: Scanner.Symbols list) n =
            let (result, _) = tail |> List.take n |> parse isDefinition isTable localScope
            (result, tail |> List.skip n)
        let expectBlock tokenList = 
            let (block, next) = getNext false isTable tokenList
            let blockContent = match block with | Block _ -> block | _ -> failwith "Expected block"        
            (blockContent, next)
        let expectNextSym sym (tail: Scanner.Symbols list) =
            let current = tail.Head
            if (current = sym) then
                (sym, tail.Tail)
            else
                failwith (sprintf "Expected symbol %A, but got %A" sym current)
        let rec expression isDef isTab token (tail: Scanner.Symbols list) orElse =
            let (next, n) = getNextSym tail
            match next with
            | SymBrace(BBraceOpen) -> 
                let (result, rest) = getNext isDef isTab n
                (result |> Some, rest)
            | SymOp(op) -> 
                let (right, n2) = getNext isDef isTab n
                let (result, prec) = getParserOperation op token
                (right |> result |> Some, n2)
            | SymPunct(PDouble) when isTable ->
                let key = token |> getNameValue
                let (tableVal, rest) = getNext true true n
                ( KeyValue(TKName(key), tableVal) |> Some,  rest)
            | SymPunct(PComma) ->
                let (nextItem, rest) = getNext isDef isTable n
                let groupToken =
                    match token with
                    | Group(group) -> Group(List.append group [nextItem])
                    | _ -> Group([token; nextItem])
                expression isDef isTable groupToken rest (Some groupToken, rest)
            | SymBrace(BBraceClose) -> (Some token, n)
            | _ -> 
                orElse
        let functionCall name (tail: Scanner.Symbols list) orElse =
            let (lookAhead, n) = getNextSym tail
            if SymBrace(BBraceOpen) = lookAhead then
                let (value, rest) = getNext false false n
                let call = FunctionCall (name, value)
                let defaultResult = (Some(FunctionCall((name, value))), rest)
                expression isDefinition isTable call rest defaultResult
            else
                orElse            
        let rec parseUntil isDef isTab stop results (restTail: Scanner.Symbols list) =
            let (nextSym, fin) = getNextSym restTail
            if (restTail.Head = stop || nextSym = stop || restTail.IsEmpty) then 
                (results |> List.rev, fin)
            else
                let (next, n) = getNext isDef isTab restTail
                let (currentResult, rest) = expression isDef isTab next n (next |> Some, n)
                parseUntil isDef isTab stop (currentResult::results) rest
        match tokens with
        | [] -> ( Some (Token (SymVal(VUnit))), [] )
        | token::tail ->
            match token with
            | SymKey(KLet) ->
                let (assign, n1) = getNext true true tail
                match assign with
                | OpBinary(OAssign, Identifier(name), value) -> (LetBinding( name, value ) |> Some, n1)
                | _ -> failwith (sprintf "Invalid assignment: %A" assign)
            | SymKey(KFun)
            | SymKey(KFunction) ->
                let (name, n1) = 
                    if token = SymKey(KFun) then
                        ( (Token(SymName(System.Guid.NewGuid().ToString()))), tail)
                    else
                        getNext true false tail

                let (argsGroup, n2) = getNext true false n1
                match argsGroup with
                | Group group ->
                    let functionName = name |> getNameValue
                    let argNames = group |> List.map getNameValue       
                    localScope <- ((functionName::localScope) |> (argNames |> List.append) )
                    let (block, rest) = getNext false false n2
                    let result = FunctionDef ( ( functionName ), argsGroup, block) |> Some
                    (result, rest)
                | _ -> failwith (sprintf "Expected arguments, but got %A (%A)" argsGroup n2) 
            | SymKey(KReturn) ->
                let (assign, n1) = getNext true true tail
                let (result, rest) = expression false false assign n1 (Some (Return (Some assign) ), n1)
                ( result, rest )
            | SymBrace(BBlockOpen) ->
                if (isDefinition && isTable) then
                    let (next, n) = (getNext true isTable tail)
                    expression true true next n (Token(SymBrace BBlockOpen) |> Some, tail)
                else
                    let (block, rest) = parseUntil isDefinition isTable (SymBrace(BBlockClose)) [] tail
                    ( Some(Block(block)), rest )
            | SymBrace(BBraceOpen) ->
                let (next, n) = getNext isDefinition isTable tail
                match next with
                | Token(SymBrace BBraceClose) -> (Group([]) |> Some, n)
                | _ -> expression isDefinition isTable (Group[next]) n ((Token token) |> Some, n)
            // | SymBrace(BBlockClose) -> ( Some(BlockStop), tail )
            | SymKey(KIf) ->
                let (_, n1) = expectNextSym (SymBrace(BBraceOpen)) tail
                let (cond, n2) = getNext false false tail
                let (ifResult, rest) = getNext false isTable n2
                let result = If(cond, ifResult, None) |> Some
                (result, rest)
            | SymName nameSymbol ->
                if isDefinition then 
                    if isTable then
                        expression true true (Identifier nameSymbol) tail ( Some(Identifier nameSymbol), tail )
                    else (Some(Identifier nameSymbol), tail)
                else                
                    if localScope |> List.contains nameSymbol then
                        functionCall nameSymbol tail ( Some(Identifier(nameSymbol)), tail )
                    else
                        failwith (sprintf "Undefined name '%s'" nameSymbol)
            | SymVal value -> 
                let defaultToken = ValueToken(value)
                let defaultResult = ( defaultToken |> Some, tail )
                if isDefinition then 
                    let lookAhead = tail.Head
                    if (SymPunct(PDouble) = lookAhead) then
                        let nameSymbol =
                            match value with
                            | VString str -> str
                            | VInteger i -> i.ToString()
                            | _ -> failwith (sprintf "Invalid table key %A" value)
                        expression true true (Identifier nameSymbol) tail defaultResult
                    else
                        defaultResult                   
                else 
                    expression false false defaultToken tail defaultResult
            // | SymBrace (BBraceOpen) ->
            //     let (content, rest) = parseUntil isDefinition (Token(SymBrace(BBraceClose))) [] tail
            //     expression (SymBrace (BBraceOpen)) 
            | _ -> ( Some(Token token), tail)

    let execute = parse false false 