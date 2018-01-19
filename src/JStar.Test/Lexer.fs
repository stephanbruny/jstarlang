namespace JStar.Test

open NUnit.Framework
open FsUnit

open JStar.Lexer

module LexerTest =
    let testLexerResult (expect : Token list) (result : Token list) =
        result
        |> List.iteri (fun i tok ->
            let expected = expect.[i]
            tok |> should equal expected
        )

    let executeNoSpaces text =
        JStar.Lexer.execute [] text
        |> List.filter (fun tok ->
            match tok with
            | TSpace _ -> false
            | _ -> true
        )

    let testExecuteLexer text expect = testLexerResult expect (executeNoSpaces text)

    [<Test>]
    let ``should convert text to lexicals`` () = 
        let text = """let something = 'Foo Bar';"""
        let result = executeNoSpaces text

        let expect = [
            TName("let");
            TName("something");
            TOperator("=");
            TString("'Foo Bar'");
            TPunctuation(";");
        ]

        testLexerResult expect result

    [<Test>]
    let ``should process empty string`` () =
        let result = JStar.Lexer.execute [] ""
        result |> should equal []

    [<Test>]
    let ``should find strings`` () =
        let string1 = @"""Foo Bar"""
        let string2 = "'Foo Bar'"
        let string3 = "`Foo Bar`"
        testExecuteLexer string1 [ TString(@"""Foo Bar""") ] 
        testExecuteLexer string2 [ TString("'Foo Bar'") ] 
        testExecuteLexer string3 [ TTemplateString("`Foo Bar`") ] 
        
    [<Test>]
    let ``should find integer and double types`` () =
        let text = "42 4.2;-123"
        let expect = [
            TInteger("42");
            TDouble("4.2");
            TPunctuation(";");
            TInteger("-123");
        ]
        testExecuteLexer text expect

    [<Test>]
    let ``should find all token types`` () =
        let text = 
            """
            function foo(x, y) {
                return x + y;
            }
            """
        let expect = [
            TName("function");
            TName("foo");
            TOpenBrace("(");
            TName("x");
            TPunctuation(",");
            TName("y");
            TCloseBrace(")");
            TOpenBrace("{");
            TName("return");
            TName("x");
            TOperator("+");
            TName("y");
            TPunctuation(";");
            TCloseBrace("}");
        ]    

        testExecuteLexer text expect