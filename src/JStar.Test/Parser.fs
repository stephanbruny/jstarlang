namespace JStar.Test

open NUnit.Framework
open FsUnit

open JStar
open JStar.Lexer
open JStar.Scanner

module ParserTest =

    let lexAndScan text =
        JStar.Lexer.execute [] text
        |> List.filter (fun tok ->
            match tok with
            | TSpace _ -> false
            | _ -> true
        )
        |> List.map JStar.Scanner.transformToken
        |> List.choose Some
        |> List.map Option.get

    [<Test>] 
    let ``should parse a let-binding`` () =
        let testCases = [
            (@"let foo = 'bar';", Some(Parser.LetBinding(("foo", VString("bar")))));
            (@"let foo = 123;", Some(Parser.LetBinding(("foo", VInteger(123)))));
            (@"let foo = 3.4;", Some(Parser.LetBinding(("foo", VDouble(3.4)))));
            (@"let foo = false;", Some(Parser.LetBinding(("foo", VBool(false)))));
            (@"let foo = true;", Some(Parser.LetBinding(("foo", VBool(true)))));
            (@"let foo = none;", Some(Parser.LetBinding(("foo", VNone))));
            (@"let foo = unit;", Some(Parser.LetBinding(("foo", VUnit))));
        ]

        testCases 
        |> List.iter(fun (text, expect) ->
            let scan = lexAndScan text
            let (result, _) = Parser.parse [] scan
            printfn "Text: %s : %A" text result
            result |> should equal expect
        )


    [<Test>] 
    let ``should fail invalid let-syntax`` () =
        let text = @"let foo != 'bar';"
        let scan = lexAndScan text
        do
            (fun () -> Parser.parse [] scan |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    let ``should parse function definition`` () =
        let text = "function myFunction(x, y) { x == y }"
        let scan = lexAndScan text
        let (result, _) = Parser.parse [] scan
        let expect = Parser.FunctionDef( "myFunction", ["x"; "y"], [] ) |> Some
        printfn "Result: %A" result
        result |> should equal expect

    [<Test>]
    let ``should parse nested blocks`` () =
        let text = "{ x { y {} } foo }"
        let scan = lexAndScan text
        let (result, _) = Parser.parse [] scan

        let blockLast = Some(Parser.Block([]))
        let blockSecond = Some(Parser.Block[ Some(Parser.Token(SymName("y"))); blockLast ]);
        let blockTop = Some(Parser.Block[ Some(Parser.Token(SymName("x"))); blockSecond; Some(Parser.Token(SymName("foo"))); ]);

        result |> should equal blockTop    
             