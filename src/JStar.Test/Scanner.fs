namespace JStar.Test

open NUnit.Framework
open FsUnit

open JStar.Lexer
open JStar.Scanner

module SymbolsTest =
    let testScannerResult (expect : Symbols option list) (result : Symbols option list) =
        do
            result.Length |> should equal expect.Length
        result
        |> List.iteri (fun i sym ->
            let expected = expect.[i]
            sym |> should equal expected
        )

    let executeNoSpaces text =
        JStar.Lexer.execute [] text
        |> List.filter (fun tok ->
            match tok with
            | TSpace _ -> false
            | _ -> true
        )

    [<Test>]
    let ``should convert doubles`` () =
        let text = @"1.2;500.66;0.999999;-33.333"
        let tokens = executeNoSpaces text
        let expect = [
            Some (SymVal(VDouble(1.2)));
            Some (SymPunct(PSemicolon));
            Some (SymVal(VDouble(500.66)));
            Some (SymPunct(PSemicolon));
            Some (SymVal(VDouble(0.999999)));
            Some (SymPunct(PSemicolon));
            Some (SymVal(VDouble(-33.333)));
        ]
        let result = tokens |> List.map (JStar.Scanner.transformToken)
        testScannerResult expect result


    [<Test>]
    let ``should transform symbols of let binding`` () =
        let text = @"let foo = 'bar', something.name;"
        let tokens = executeNoSpaces text
        let expect = [
            Some (SymKey(KLet));
            Some (SymName("foo"));
            Some (SymOp(OAssign));
            Some (SymVal(VString("bar")));
            Some (SymPunct(PComma));
            Some (SymName("something"));
            Some (SymPunct(PAccess));
            Some (SymName("name"))
            Some (SymPunct(PSemicolon));
        ]
        let result = tokens |> List.map (JStar.Scanner.transformToken)
        testScannerResult expect result