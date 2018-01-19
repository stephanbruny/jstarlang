namespace JStar

open JStar.Lexer
open JStar.Scanner
open System.Reflection.Emit

module Parser =

    type Parselet = 
    | LetBinding of string * Scanner.Value
    | FunctionDef of string * (string list) * (Parselet list) // function of name * parameters * body
    | Block of Parselet option list
    | BlockStop
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

    let getStaticValue valueToken =
        let errorMessage = sprintf "Unexpected token %A" valueToken
        match valueToken with
        | Token scan -> 
            match scan with
            | SymVal value -> value
            | _ -> failwith errorMessage    
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

    let rec parse scope (tokens : Scanner.Symbols list) =
        let tail = tokens |> List.tail
        let getNext tokenList =
            let (result, rest) = parse scope tokenList
            (result |> someOrError "Expected name", rest)
        match tokens with
        | [] -> ( Some (Token (SymVal(VUnit))), tail )
        | _ ->
            let token = (tokens |> List.head)
            match token with
            | SymKey(KLet) ->
                let (name, n1) = getNext (tokens |> List.tail)
                let (op, n2) = getNext n1
                expectAssign op |> ignore
                let (value, n3) = getNext n2
                let result = LetBinding ( (name |> getNameValue ), (value |> getStaticValue) ) |> Some
                ( result, n3 )
            | SymKey(KFunction) ->
                let (name, n1) = getNext (tokens |> List.tail)
                let (openBrace, n2) = getNext n1
                do expectSym (SymBrace(BBraceOpen)) openBrace |> ignore
                let stop = SymBrace(BBraceClose)
                let paramTokens = n2 |> List.takeWhile(fun tk -> not (isSym stop (Token tk)) )
                let fnParams = 
                    paramTokens 
                    |> List.map(fun tk ->
                        let (result, _) = parse scope [tk]
                        result
                    ) 
                    |> List.choose id
                    |> filterCommas
                    |> extractParamNames
                let n3 = n2 |> List.skip fnParams.Length
                let result = FunctionDef ( ( name |> getNameValue ), fnParams, []) |> Some
                (result, n3)
            | SymBrace(BBlockOpen) ->
                let mutable currentParselet = Some(Token token)
                let mutable result : Parselet option list = []
                let mutable restTail = tail
                let isDone () = (currentParselet = Some(BlockStop)) || (restTail.IsEmpty)
                while (not (isDone())) do
                    let (res, rest) = parse scope restTail
                    if (res <> Some(BlockStop)) then result <- res::result
                    currentParselet <- res
                    restTail <- rest
                ( Some(Block(result |> List.rev)), restTail)                
            | SymBrace(BBlockClose) -> ( Some(BlockStop), tail )
            | _ -> ( Some(Token token), tail)

