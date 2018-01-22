namespace JStar

open JStar.Lexer
open Microsoft.VisualBasic.CompilerServices
open Microsoft.VisualBasic.CompilerServices
open Microsoft.VisualBasic.CompilerServices

module Scanner =

    type Keywords =
    | KLet
    | KVar
    | KFunction
    | KFun
    | KIf
    | KForeach
    | KWhile
    | KRepeat
    | KBreak
    | KMatch
    | KWith
    | KAs
    | KAsync
    | KAwait
    | KReceive
    | KSend
    | KExtend
    | KTry
    | KCatch
    | KReturn

    type Braces =
    | BBlockOpen
    | BBlockClose
    | BBraceOpen
    | BBraceClose
    | BBracketOpen
    | BBracketClose

    type Operators =
    | OCompareEqual /// ==
    | OCompareUnequal /// !=
    | OAssign /// let foo = 'bar'
    | OAssignMutable /// mutable <- value
    | OArrow /// fun () => ...
    | OCustom of string

    type Punct =
    | PSemicolon /// some code...;
    | PDouble /// table.something
    | PComma /// (a, b, "foo")
    | PAccess

    type Value =
    | VInteger of int
    | VDouble of double
    | VString of string
    | VTemplateString of string
    | VReference of string
    | VCall of string * (Value list)
    | VOp of Operators
    | VExpression of Value list
    | VBool of bool
    | VUnit
    | VNone
    | VTable of (string * Value) list

    type Symbols =
    | SymKey of Keywords
    | SymBrace of Braces
    | SymOp of Operators
    | SymPunct of Punct
    | SymVal of Value
    | SymName of string
    | SymUnexpected of string

    /// Removes quotes from String Token
    let getStringContent (str : string) = str.Substring(1, str.Length - 2)

    let lookAhead (tokenStack : Lexer.Token list) (expect : Lexer.Token list) (next : Lexer.Token) =
        if expect |> List.contains next then 
            Some (next::tokenStack)
        else
            None // TODO: Build proper error message (token name)
    let matchOperator op =
        match op with
        | "==" -> SymOp(OCompareEqual)
        | "!=" -> SymOp(OCompareUnequal)
        | "=" -> SymOp(OAssign)
        | "<-" -> SymOp(OAssignMutable)
        | "=>" -> SymOp(OArrow)
        | _ -> SymOp(OCustom(op))

    let matchBrace brace =
        match brace with
        | "{" -> SymBrace(BBlockOpen)    
        | "}" -> SymBrace(BBlockClose)
        | "(" -> SymBrace(BBraceOpen)
        | ")" -> SymBrace(BBraceClose)
        | "[" -> SymBrace(BBracketOpen)
        | "]" -> SymBrace(BBracketClose)
        | _ -> SymUnexpected(brace)
        
    let matchPunct punct =
        match punct with
        | "." -> SymPunct(PAccess)
        | ";" -> SymPunct(PSemicolon)
        | ":" -> SymPunct(PDouble)
        | "," -> SymPunct(PComma)
        | _ -> SymUnexpected(punct)

    let matchName name =
        match name with
        | "let" -> SymKey(KLet)
        | "var" -> SymKey(KVar)
        | "function" -> SymKey(KFunction)
        | "fun" -> SymKey(KFun)
        | "if" -> SymKey(KIf)
        | "while" -> SymKey(KWhile)
        | "foreach" -> SymKey(KForeach)
        | "repeat" -> SymKey(KRepeat)
        | "break" -> SymKey(KBreak)
        | "match" -> SymKey(KMatch)
        | "with" -> SymKey(KWith)
        | "as" -> SymKey(KAs)
        | "async" -> SymKey(KAsync)
        | "await" -> SymKey(KAwait)
        | "receive" -> SymKey(KReceive)
        | "send" -> SymKey(KSend)
        | "extend" -> SymKey(KExtend)
        | "try" -> SymKey(KTry)
        | "catch" -> SymKey(KCatch)
        | "return" -> SymKey(KReturn)
        | "unit" -> SymVal(VUnit)
        | "none" -> SymVal(VNone)
        | "true" -> SymVal(VBool(true))
        | "false" -> SymVal(VBool(false))
        | _ -> SymName(name)

    let parseDouble (str : string) =
        System.Double.Parse(str, System.Globalization.NumberFormatInfo.InvariantInfo)

    let transformToken token =
        match token with
        | TComment _ -> None
        | TOperator op -> Some (matchOperator op)
        | TOpenBrace brace
        | TCloseBrace brace -> Some (matchBrace brace)
        | TName name -> Some (matchName name)
        | TDouble str -> Some (SymVal(VDouble( parseDouble str )))
        | TInteger str -> Some (SymVal(VInteger( System.Convert.ToInt32 str )))
        | TString str -> Some (SymVal(VString( getStringContent str )))
        | TTemplateString str -> Some (SymVal(VTemplateString( getStringContent str )))
        | TPunctuation punct -> Some (matchPunct punct)
        | TNewLine -> None
        | TSpace _ -> None
        | TEnd -> None

    let transformTokenLine token =
        match token with
        | TNewLine -> (None, 1)
        | _ -> ((transformToken token), 0)