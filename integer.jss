/* Integer Module */
export {
    getType: fun () { Types.Integer },
    toString: fun () { System.Call('toString')(this.@value) },
    operator (+): fun (x) {
        match x.getType() as t with
        | Types.Integer: System.Call('add')(this.@value, x);
        | Types.Float: Float(x) + this.@value;
        | _: throw Exception(`Cannot add ${t.toString()} to integer`);
    },
    operator (-): fun (x) {
        match x.getType() as t with
        | Types.Integer: System.Call('substract')(this.@value, x);
        | Types.Float: Float(x) - this.@value;
        | _: throw Exception(`Cannot substract ${t.toString()} from integer`);
    },
    operator (*): fun (x) {
        match x.getType() as t with
        | Types.Integer: System.Call('multiply')(this.@value, x);
        | Types.Float: Float(x) * this.@value;
        | _: throw Exception(`Cannot multiply ${t.toString()} with integer`);
    },
    operator (/): fun (x) {
        match x.getType() as t with
        | Types.Integer: System.Call('divideInteger')(this.@value, x);
        | Types.Float: Float(x) / this.@value;
        | _: throw Exception(`Cannot divide integer by ${t.toString()}`);
    },
    operator (==) : fun (x) {
        match x.getType() with
        | Types.Integer: System.Call('compareInt')(this.@value, x);
        | _: false
    },
    operator (!=) : fun (x) {
        not this == x;
    }
}