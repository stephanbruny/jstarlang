namespace JStar

module NativeTypes =

    type Reference = Reference of string

    type StringAtom = StringAtom of string
    type IntegerAtom = IntegerAtom of int
    type FloatAtom = FloatAtom of float32
    type BoolAtom =  BoolAtom of bool

    type Value<'T> =
    | Dynamic of Reference
    | Static of 'T


    type AtomicType = 
    | StringType of Value<StringAtom>
    | IntegerType of Value<IntegerAtom>
    | FloatType of Value<FloatAtom>
    | BoolType of Value<BoolAtom>

    type PropertyName = PropertyName of string

    type Properties = Properties of Map<PropertyName, AtomicType>

    type Table = Table of Reference * Properties

    type TableValue = TableValue of Value<Table>

    type TableType = 
    | StringTable
    | IntegerTable
    | FloatTable
    | BoolTable
    | TupleTable
    | CollectionTable

    let property name (value : AtomicType) = (PropertyName(name), value)

    let guid () = System.Guid.NewGuid().ToString()

    let createSimpleValueTable ref (value : AtomicType) = 
        let properties = [ (PropertyName("@value"), value) ] |> Map.ofList
        Table( Reference(ref), Properties(properties) )

    let createNewTable value = createSimpleValueTable (guid()) value
    let createStringTable text = createNewTable (StringType(text))
    let createIntegerTable value = createNewTable (IntegerType(value))

    let stringAtomStatic value = (Value<StringAtom>.Static (StringAtom value))
    let integerAtomStatic value = (Value<IntegerAtom>.Static (IntegerAtom value))
    let floatAtomStatic value = (Value<FloatAtom>.Static (FloatAtom value))
    let boolAtomStatic value = (Value<BoolAtom>.Static (BoolAtom value))
    let foo = createStringTable (stringAtomStatic "foo")