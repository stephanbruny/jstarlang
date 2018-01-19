// Learn more about F# at http://fsharp.org

open System
open JStar
open JStar.Lexer

[<EntryPoint>]
let main argv =

    let test = 
        """
        let text = "Foo and bar and so on";
        let foo = 1234 + 1.3;
        var mutable = '';
        mutable <- 'some value';
        function hello(text) {
            Console.WriteLine(`Hello, ${text}!`);
        }
        
        hello('foo');
        """

    let tokens = 
        Lexer.execute [] test
        |> List.filter (fun tok ->
            match tok with
            | TSpace _ -> false
            | _ -> true
        )
    
    printfn "Token: %A" tokens

    printfn "Hello World from F#!"
    0 // return an integer exit code
