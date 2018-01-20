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
            let (result, _) = Parser.execute [] scan
            printfn "Text: %s : %A" text result
            result |> should equal expect
        )

    [<Test>] 
    let ``should parse a table`` () =
        let text = "let foo = { x: 3 bar: { y: 'inner', z: 4.2 } }"
        let scan = lexAndScan text
        let barTable = ("bar", VTable [ ("y", VString("inner")); ("z", VDouble(4.2)) ])
        let fooTable = VTable [ ("x", VInteger(3)); barTable ]
        let expect = Some(Parser.LetBinding("foo", fooTable))

        let (result, _) = Parser.execute [] scan
        result |> should equal expect

    [<Test>]
    let ``should parse table with string and integer keys`` () =
        let text = "let table = { 'foo': 'bar', 'bar': 12.34, 3: unit }" 
        let expect = Parser.LetBinding("table", VTable[ ("foo", VString "bar"); ("bar", VDouble 12.34); ("3", VUnit) ]) |> Some
        let scan = lexAndScan text
        let (result, _) = Parser.execute [] scan
        result |> should equal expect

    [<Test>] 
    let ``should fail invalid let-syntax`` () =
        let text = @"let foo != 'bar';"
        let scan = lexAndScan text
        do
            (fun () -> Parser.execute [] scan |> ignore) |> should throw typeof<System.Exception>

    [<Test>]
    let ``should parse function definition`` () =
        let text = "function myFunction(x, y) { x == y }"
        let scan = lexAndScan text
        let (result, _) = Parser.execute [] scan
        let fnBlock = [ Some(Parser.Reference("x")); Some(Parser.Token(SymOp(OCompareEqual))); Some(Parser.Reference("y")); ]
        let expect = Parser.FunctionDef( "myFunction", ["x"; "y"], fnBlock ) |> Some
        printfn "Result: %A" result
        result |> should equal expect

    [<Test>]
    let ``should parse nested blocks`` () =
        let text = "{ x { y {} } foo }"
        let scan = lexAndScan text
        let (result, _) = Parser.execute [ "x"; "y"; "foo" ] scan

        let blockLast = Some(Parser.Block([]))
        let blockSecond = Some(Parser.Block([ Some(Parser.Reference("y")); blockLast ]));
        let blockTop = Some(Parser.Block([ Some(Parser.Reference("x")); blockSecond; Some(Parser.Reference("foo")); ]));

        result |> should equal blockTop

    [<Test>]
    let ``should parse recursive function code`` () =
        let text = 
            """
            function fib(n) {
                if (n <= 1) { return n; }
                return fib(n - 1) + fib(n - 2);
            }
            """

        let ifCond = [ Some(Parser.Reference("n")); Some(Parser.Token(SymOp(OCustom("<=")))); Some(Parser.ValueToken(VInteger(1))) ]
        let ifBlock = [ Some(Parser.Token(SymKey(KReturn))); Some(Parser.Reference("n")); Some(Parser.Token(SymPunct(PSemicolon))) ]
        let fnRest = [
            Some(Parser.Token(SymKey(KReturn)));
            Some(Parser.Reference("fib"));
            Some(Parser.Token(SymBrace(BBraceOpen)));
            Some(Parser.Reference("n"));
            Some(Parser.Token(SymOp(OCustom("-"))));
            Some(Parser.ValueToken(VInteger(1)));
            Some(Parser.Token(SymBrace(BBraceClose)));
            Some(Parser.Token(SymOp(OCustom("+"))));
            Some(Parser.Reference("fib"));
            Some(Parser.Token(SymBrace(BBraceOpen)));
            Some(Parser.Reference("n"));
            Some(Parser.Token(SymOp(OCustom("-"))));
            Some(Parser.ValueToken(VInteger(2)));
            Some(Parser.Token(SymBrace(BBraceClose)));
            Some(Parser.Token(SymPunct(PSemicolon)));
        ]
        let ifPart = Some(Parser.If(ifCond, ifBlock))

        let expect = Some(Parser.FunctionDef("fib", ["n"], (ifPart::fnRest)))

        let scan = lexAndScan text
        let (result, _) = Parser.execute [] scan
        result |> should equal expect

    [<Test>]
    let ``should parse a more complex example`` () =
        let text = 
            """
            let a = 1;
            let b = "b";
            function foo(x) { return x + a }
            let table = { foo: b }
            """
        let scan = lexAndScan text
        let rec exec result scope tokens =
            let (part, rest) = Parser.execute scope tokens
            let localScope =
                match part with
                | Some(Parser.FunctionDef(name, _ , _))
                | Some(Parser.LetBinding(name, _)) -> name::scope
                | _ -> scope
            let partialResult = part::result
            if rest.IsEmpty then
                partialResult |> List.rev
            else
                exec partialResult localScope rest
        let result = exec [] [] scan

        printfn ("Result: %A") result

        let expectFn = [
            Some(Parser.Token(SymKey KReturn));
            Some(Parser.Reference("x"));
            Some(Parser.Token(SymOp(OCustom ("+"))));
            Some(Parser.Reference("a"))
        ]

        let expect = [
            Some (Parser.LetBinding("a", VInteger(1)));
            Some (Parser.Token(SymPunct(PSemicolon)));
            Some (Parser.LetBinding("b", VString("b")));
            Some (Parser.Token(SymPunct(PSemicolon)));
            Some (Parser.FunctionDef("foo", ["x"], expectFn)); // TODO
            Some (Parser.LetBinding("table", VTable([ ("foo", VReference("b")) ])));
        ]

        result |> List.iteri(fun i res -> res |> should equal expect.[i])