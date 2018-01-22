namespace JStar

open System.Text.RegularExpressions
open System

module Lexer =
    let (|RegexMatch|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    type Token =
    | TName of string
    | TInteger of string
    | TDouble of string
    | TString of string
    | TTemplateString of string
    | TSpace of string
    | TOperator of string
    | TPunctuation of string
    | TOpenBrace of string
    | TCloseBrace of string
    | TNewLine
    | TComment // do we need this???
    | TEnd

    let getToken snippet =
        match snippet with
        | RegexMatch @"^(\/\/.*)[$|\n]*" [tok] -> (TComment, tok)
        | RegexMatch @"^(/\*(?:(?!\*/)(?:.|[\r\n]+))*\*/)" [tok] -> (TComment, tok)
        | RegexMatch @"^([a-zA-Z_]\w*)" [tok] -> (TName(tok), tok)
        | RegexMatch @"^(\s+)" [tok] -> (TSpace(tok), tok)
        | RegexMatch @"^(\-?\d+\.\d+)" [tok] -> (TDouble(tok), tok)
        | RegexMatch @"^(\-?\d+)" [tok] -> (TInteger(tok), tok)
        | RegexMatch @"^([\.|\;|\:|\,])" [tok] -> (TPunctuation(tok), tok)
        | RegexMatch @"^(\(|\{|\[)" [tok] -> (TOpenBrace(tok), tok)
        | RegexMatch @"^(\)|\}|\])" [tok] -> (TCloseBrace(tok), tok)
        | RegexMatch @"^(\'[^\']*\')" [tok] -> (TString(tok), tok)
        | RegexMatch @"^(\""[^\""]*\"")" [tok] -> (TString(tok), tok)
        | RegexMatch @"^(\`[^\`]*\`)" [tok] -> (TTemplateString(tok), tok)
        | RegexMatch @"^([^\w\s]+)" [tok] -> (TOperator(tok), tok)
        | RegexMatch @"\n" [tok] -> (TNewLine, tok)
        | _ -> (TEnd, String.Empty)

    let rec execute result text =
        let (token, part) = getToken text
        match token with
        | TEnd -> result |> List.rev
        | _ ->
            let len = Math.Max(part.Length, 1)
            let next = text.Substring(len)
            execute (token::result) next