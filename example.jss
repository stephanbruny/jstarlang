/** NATIVE TYPES **/
- Unit
- Integer
- Float
- String
- Bool
- Tuple
- Table
- Function
- Exception*
- Process*

* - special Table Type

`lol ${xyz.toString()}`

let n = 1; // integer
let f = 1.2; // float
let text = "foo"; // string
let tup = 1, 2; // tuple
let table = { // table
    foo: 'bar',
    tab: { name: 'another-table' },
    someFun: fun () { Console.WriteLine(this.foo) } // this = table, shortcut for table's symbol name
}
let enum = | Something, SomethingElse, Foo, Bar |;

// Mutables
var mutable = 0;
mutable <- 123; // type is set on initialization
mutable <- 'foo'; // error - cannot assign type string to variable of type integer

// Missing Values
let nothing = none; // No null or undefined
let something = some(123);

match variable with
| none: `Nothing`
| some: `Something: ${variable.toString()}`

- **some** matches any type with a set value
- **none** matches any missing value

// decompose tuple
let (x, y) = tup; // (1, 2)

// decompose table
let { foo } = table; // 'bar'

/** FUNCTIONS **/
function example (a, b, c) {
    let result = 
        if (a > 0) b;
        if (a < 0) c;
        a;
    return result;
}

// lambda
fun (a, b, c) { ... }

/** LOOPS **/
foreach (table as key, value) {

}

while (/* condition */) {

}

repeat {
    ...
    break;
}

/** TABLES **/
// access table
let tabName = table.tab.name;
let tabName = table['tab']['name'];

// Array-Like
let list = { 0: 'foo', 1: 'bar', 2: 'foobar' }
let list = Array ('foo', 'bar', 'foobar')

// Extend
let table2 = extend table {
    foo: 'foo foo', // override when key exists
    bar: 42,
    parent: table // reference to parent for simulating inheritance
}

// Keys
- string
- integer
- enum

/** PROCESS (Actor Model) **/
let FooProcess = receive (a, b) {
    Console.WriteLine(`${a} :: ${b}`);
}

let BarProcess = receive (pid) {
    send pid ('foo', 'bar')
}

send BarProcess (FooProcess);

/** ASYNC **/
let somethingAsync = async {
    let x = 123;
    let y = await anotherAsync (); // execution paused until value received
    return x + y;
}

/** PATTERN MATCHING **/
let someValue = 
    match foo with
    | 'bar': 'foo bar';
    | 'foo': 'foo foo';
    | _: 'just foo';

let fizzBuzz =
    match number as n with
    | n % 3 == 0 : 'Fizz';
    | n % 5 == 0 : 'Buzz';
    | n % 3 == 0 and n % 5 == 0 : 'FizzBuzz';
    | _: n.toString();

/** MODULES **/
- any script file is a module

// Exporting
- export is unnamed
- default is none
export value;

// Importing
- imports are assignments

let MyModule = import './path/to/my-module';

/** BUILT-IN REFLECTION AND META-PROGRAMMING **/
// module custom-type.j
let CustomType = {
    toString: fun () {
        `CustomType(${ this.@value.toString() })`
    },
    @let: fun (value) {
        match value with
        | some: this.@value <- value;
        | none: throw Exception('Cannot create CustomType with none value');
    },
    operator (>>) : fun (x) {
        match x.getType() as with
        | Types.Function: x(@value.toString());
        | _: throw Exception(`Cannot forward CustomType to type ${t.toString()}`)
    }
}

export fun (value) {
    extend $CustomType { @value: value }
}
// Some other module
let CustomType = import './custom-type';
let custom = CustomType('foo bar');
custom >> Console.WriteLine;

/** EXCEPTION HANDLING */
function error(message){
    throw Exception(message);
}

try {
    error('Crash');
} catch as ex {
    match ex.getType() as t with
    | Types.Exception: Console.WriteLine(`Exception caught ${ex.message}`);
    | _: Console.WriteLine(`Something went wrong, caught ${t.toString()}`);
}

function io() {
    do {
        a.b.noResult();
    }
    return unit;
}