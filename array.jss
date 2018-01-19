let ArrayType = {
    @value: { @mutable: true },
    @access: fun (key) {
        match key.getType() with
        | Types.Integer: this.@value[key]
        | _: this[key];
    },
    slice: fun (start, end) {
        let indices = start ... end;
        let result = ArrayType();
        var destIndex = 0;
        foreach ( indices as index ) {
            result[destIndex] = this.@value[index];
            destIndex <- destIndex + 1;
        }
        result;
    },
    push: fun(value) {
        let index = this.@value.@length + 1;
        this.@value[index] <- value;
    }
}

export fun (values) {
    let array = ArrayType();
    var index = 0;
    foreach (values as value) {
        array[index] = value;
        index <- index + 1;
    }
    extend array { @mutable: false };
}