namespace JStar

open JStar.Lexer
open JStar.Scanner
open System.Reflection.Emit

module Parser =

    type ReferenceType =
    | RefConstant of string
    | RefMutable of string
    | RefFunction of string

    type ExpressionValue = 
    | ValReference of string
    | ValStatic of Scanner.Value
    | ValSubtable of ExpressionValue list

    type TableKey =
    | TKNumeric of int
    | TKName of string

    type TableProperty = TableProperty of TableKey * ExpressionValue

    type TableDefinition = 
    | TableProp of TableProperty
    | SubTable of TableDefinition

    type Parselet = 
    | LetBinding of string * Scanner.Value
    | FunctionDef of string * (string list) * (Parselet option list) // function of name * parameters * body
    | If of (Parselet option list) * (Parselet option list)
    | Block of Parselet option list
    | BlockStop
    | Reference of string
    | TableDef of TableDefinition list
    | ValueToken of Scanner.Value
    | KeyValue of TableKey * Scanner.Value
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
        | Token scan -> 
            match scan with
            | SymName value -> value
            | _ -> failwith errorMessage
        | _ -> failwith errorMessage 


    let rec getStaticValue valueToken =
        let errorMessage = sprintf "Unexpected token in assignment: %A" valueToken
        match valueToken with
        | Block content ->
            let tableData = 
                content 
                |> List.filter(fun p -> (p <> Some(Token(SymPunct(PComma)))))
                |> List.map(fun parselet ->
                    match parselet with
                    | Some (KeyValue ( TKName(n), v)) -> (n, v)
                    | _ -> failwith (sprintf "Unexpected token in table: %A" parselet)
                )
            VTable(tableData)
        | ValueToken value -> value
        | _ -> failwith errorMessage           

    let getOperator opToken =
        let errorMessage = sprintf "Expected opertor token, but got %A" opToken
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

    let isSym sym token =        
        let tok = expectToken token
        tok = sym

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
        tokens |> List.filter( fun tk -> not (isSym (SymPunct(PComma)) tk) )

    let rec parse isDefinition scope (tokens : Scanner.Symbols list) =
        let mutable localScope = scope
        let rec parseUntil isDef stop results restTail =
            let (res, rest) = parse isDef localScope restTail
            if (res <> Some(stop) || restTail.IsEmpty) then 
                parseUntil isDef stop (res::results) rest
            else
                (results |> List.rev, rest)
        let getNext isDef tokenList =
            let (result, rest) = parse isDef localScope tokenList
            (result |> someOrError "Expected name", rest)
        let expectBlockContent tokenList = 
            let (block, next) = getNext false tokenList
            let blockContent = match block with | Block content -> content | _ -> failwith "Expected block"        
            (blockContent, next)
        let defineTable nameSymbol tail orElse = 
            let (lookAhead, n) = getNext false tail
            let doub = SymPunct(PDouble)
            if (isSym doub lookAhead) then
                let isDef = n.Head = SymBrace(BBlockOpen) 
                let (value, next) = getNext isDef n
                match value with
                | ValueToken v -> ( Some( KeyValue( TKName(nameSymbol), v ) ), next )
                | Reference ref -> ( Some( KeyValue( TKName(nameSymbol), VReference(ref) ) ), next )
                | Block _ -> 
                    let pairValue = getStaticValue value
                    ( Some(KeyValue(TKName(nameSymbol), pairValue)), next )
                | _ -> failwith (sprintf "Unexpected table definition %A (in table %s)" value nameSymbol)    
            else
                orElse        
        match tokens with
        | [] -> ( Some (Token (SymVal(VUnit))), [] )
        | token::tail ->
            match token with
            | SymKey(KLet) ->
                let (name, n1) = getNext true tail
                let (op, n2) = getNext false n1
                expectAssign op |> ignore
                let (value, n3) = getNext true n2
                let result = LetBinding ( (name |> getNameValue ), (value |> getStaticValue) ) |> Some
                ( result, n3 )
            | SymKey(KFunction) ->
                let (name, n1) = getNext true tail
                let (openBrace, n2) = getNext false n1
                do expectSym (SymBrace(BBraceOpen)) openBrace |> ignore
                let stop = SymBrace(BBraceClose)
                let (paramTokens, n3) = parseUntil true (Token stop) [] n2
                let fnParams = 
                    paramTokens
                    |> List.choose id
                    |> filterCommas
                    |> extractParamNames
                let functionName = name |> getNameValue                
                localScope <- ((functionName::localScope) |> List.append fnParams)
                let (blockContent, rest) = expectBlockContent n3
                let result = FunctionDef ( ( functionName ), fnParams, blockContent) |> Some
                (result, rest)
            | SymBrace(BBlockOpen) ->
                let (block, rest) = parseUntil isDefinition BlockStop [] tail
                ( Some(Block(block)), rest )
            | SymBrace(BBlockClose) -> ( Some(BlockStop), tail )
            | SymKey(KIf) ->
                let (openBrace, n1) = getNext false tail
                do expectSym (SymBrace(BBraceOpen)) openBrace |> ignore
                let (cond, n2) = parseUntil false (Token (SymBrace(BBraceClose))) [] n1
                let (blockContent, rest) = expectBlockContent n2
                let result = If(cond, blockContent) |> Some
                (result, rest)
            | SymName nameSymbol ->
                if isDefinition then 
                    defineTable nameSymbol tail ( Some(Token token), tail )
                else                
                    if localScope |> List.contains nameSymbol then
                        ( Some(Reference(nameSymbol)), tail )
                    else
                        failwith (sprintf "Undefined name '%s'" nameSymbol)
            | SymVal value -> 
                let defaultResult = ( Some(ValueToken(value)), tail )
                if isDefinition then 
                    let lookAhead = tail.Head
                    if (SymPunct(PDouble) = lookAhead) then
                        let nameSymbol =
                            match value with
                            | VString str -> str
                            | VInteger i -> i.ToString()
                            | _ -> failwith (sprintf "Invalid table key %A" value)
                        defineTable nameSymbol tail defaultResult
                    else
                        defaultResult                   
                else 
                    defaultResult
            | _ -> ( Some(Token token), tail)

    let execute = parse false        