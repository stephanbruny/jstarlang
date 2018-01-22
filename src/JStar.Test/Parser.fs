namespace JStar.Test

open NUnit.Framework
open FsUnit

open JStar
open JStar.Lexer
open JStar.Scanner
open JStar.Parser

module ParserTest =

    let lexAndScan text =
        JStar.Lexer.execute [] text
        |> List.filter (fun tok ->
            match tok with
            | TSpace _ -> false
            | _ -> true
        )
        |> List.map JStar.Scanner.transformToken
        |> List.choose id

    [<Test>] 
    let ``should parse a let-binding`` () =
        let testCases = [
            (@"let foo = 'bar';", Some(Parser.LetBinding(("foo", Parser.ValueToken(VString("bar"))))));
            (@"let foo = 123;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VInteger(123))))));
            (@"let foo = 3.4;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VDouble(3.4))))));
            (@"let foo = false;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VBool(false))))));
            (@"let foo = true;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VBool(true))))));
            (@"let foo = none;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VNone)))));
            (@"let foo = unit;", Some(Parser.LetBinding(("foo", Parser.ValueToken(VUnit)))));
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
        let text = "let foo = { x: 3, bar: { y: 'inner', z: 4.2 } }"
        let scan = lexAndScan text
        let barTable = 
            [
                Parser.KeyValue(Parser.TKName("y"), Parser.ValueToken(VString("inner")));
                Parser.KeyValue(Parser.TKName("z"), Parser.ValueToken(VDouble(4.2)));
            ] 
            |> Parser.Group
        let fooTable = 
            [
                Parser.KeyValue(Parser.TKName("x"), Parser.ValueToken(VInteger(3)));
                Parser.KeyValue(Parser.TKName("bar"), barTable);
            ]
            |> Parser.Group

        let expect = Some(Parser.LetBinding("foo", fooTable))

        let (result, _) = Parser.execute [] scan
        result |> should equal expect

    [<Test>]
    let ``should parse table with string and integer keys`` () =
        let text = "let table = { 'foo': 'bar', 'bar': 12.34, 3: unit }" 
        let tableContents = [
            Parser.KeyValue(Parser.TKName("foo"), Parser.ValueToken(VString("bar")));
            Parser.KeyValue(Parser.TKName("bar"), Parser.ValueToken(VDouble(12.34)));
            Parser.KeyValue(Parser.TKName("3"), Parser.ValueToken(VUnit));
        ]
        let expect = Parser.LetBinding("table", Group(tableContents)) |> Some
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
        let fnBlock = [ Some(Parser.OpBinary(OCompareEqual, Parser.Identifier("x"), Parser.Identifier("y"))) ]
        let expect = Parser.FunctionDef( "myFunction", Group [Identifier "x"; Identifier "y"], Parser.Block(fnBlock) ) |> Some
        printfn "Result: %A" result
        result |> should equal expect

    [<Test>]
    let ``should parse function without parameters`` () =
        let text = "function myFunction() { 'funky' }"
        let scan = lexAndScan text
        let (result, _) = Parser.execute [] scan
        let expect = Parser.FunctionDef( "myFunction", Group [], Block [Some(Parser.ValueToken(VString("funky")))] ) |> Some
        printfn "Result: %A" result
        result |> should equal expect

    // [<Test>]
    // let ``should parse nested blocks`` () =
    //     let text = "{ x { y {} } foo }"
    //     let scan = lexAndScan text
    //     let (result, _) = Parser.execute [ "x"; "y"; "foo" ] scan

    //     let blockLast = Some(Parser.Block([]))
    //     let blockSecond = Some(Parser.Block([ Some(Parser.Identifier("y")); blockLast ]));
    //     let blockTop = Some(Parser.Block([ Some(Parser.Identifier("x")); blockSecond; Some(Parser.Identifier("foo")); ]));

    //     result |> should equal blockTop

    [<Test>]
    let ``should parse recursive function code`` () =
        let text = 
            """
            function fib(n) {
                if (n <= 1) { return n; }
                return fib(n - 1) + fib(n - 2);
            }
            """

        let fnRest = [
            Some
                (If
                   (OpBinary (OCustom "<=",Identifier "n",ValueToken (VInteger 1)),
                    Block
                      [Some (Return (Some (Identifier "n")));
                       Some (Token (SymPunct PSemicolon))],None));
              Some
                (OpBinary
                   (OCustom "+",
                    OpBinary (OCustom "-",Identifier "n",ValueToken (VInteger 1)),
                    OpBinary
                      (OCustom "-",FunctionCall ("fib",Identifier "n"),
                       ValueToken (VInteger 2)))); Some (Token (SymPunct PSemicolon))
        ]

        let expect = Some(Parser.FunctionDef("fib", Group [Identifier "n"], Parser.Block(fnRest)))

        let scan = lexAndScan text
        let (result, _) = Parser.execute [] scan
        result |> should equal expect

    // [<Test>]
    // let ``should parse a more complex example`` () =
    //     let text = 
    //         """
    //         // Line Comment
    //         let a = 1; // A
    //         let b = "b"; // B
    //         /**
    //             foo function
    //         **/
    //         function foo(x) { return x + a }
    //         let table = { foo: b }
    //         """
    //     let scan = lexAndScan text

    //     let rec exec result scope tokens =
    //         let (part, rest) = Parser.execute scope tokens
    //         let localScope =
    //             match part with
    //             | Some(Parser.FunctionDef(name, _ , _))
    //             | Some(Parser.LetBinding(name, _)) -> name::scope
    //             | _ -> scope
    //         let partialResult = part::result
    //         if rest.IsEmpty then
    //             partialResult |> List.rev
    //         else
    //             exec partialResult localScope rest
    //     let result = exec [] [] scan

    //     let expectFn = [
    //         Some(Parser.Token(SymKey KReturn));
    //         Some(Parser.Identifier("x"));
    //         Some(Parser.Token(SymOp(OCustom ("+"))));
    //         Some(Parser.Identifier("a"))
    //     ]

    //     let expect = [
    //         Some (Parser.LetBinding("a", VInteger(1)));
    //         Some (Parser.Token(SymPunct(PSemicolon)));
    //         Some (Parser.LetBinding("b", VString("b")));
    //         Some (Parser.Token(SymPunct(PSemicolon)));
    //         Some (Parser.FunctionDef("foo", ["x"], Parser.Block(expectFn))); // TODO
    //         Some (Parser.LetBinding("table", VTable([ ("foo", VReference("b")) ])));
    //     ]

    //     result |> List.iteri(fun i res -> res |> should equal expect.[i])

    // [<Test>]
    // let ``should parse anonymous function definition/call`` () =
    //     let text = "(fun (foo) { sprint('Hello') })('Bar')"
    //     let scan = lexAndScan text
    //     let result = Parser.execute ["sprint"] scan
    //     printfn "Result: %A" result
    //     result |> should equal []