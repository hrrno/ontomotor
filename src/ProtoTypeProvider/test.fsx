//#r @".\bin\Debug\Samples.HelloWorldTypeProvider.dll"
//
//
//let obj1 = Samples.HelloWorldTypeProvider.Type1("some data")
//
//let obj2 = Samples.HelloWorldTypeProvider.Type1("some other data")
//
//obj1.InstanceProperty
//obj2.InstanceProperty
//
//[ for index in 0 .. obj1.InstanceProperty-1 -> obj1.InstanceMethod(index) ]
//[ for index in 0 .. obj2.InstanceProperty-1 -> obj2.InstanceMethod(index) ]
//
//let data1 = Samples.HelloWorldTypeProvider.Type1.NestedType.StaticProperty35

open System
open System.Text.RegularExpressions
open System.Globalization

let (|IsMatch|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some str
    else None


let doit contento =
    let (type':Type, content:obj) =
        match contento with
        | c when c = "true" || c = "false" -> 
            typeof<bool>, bool.Parse(c) :> obj
        | IsMatch "(\d{1,4})-(\d{1,2})-(\d{1,2})" c ->
            typeof<DateTime>, DateTime.ParseExact(c, "yyyy-MM-dd", CultureInfo.InvariantCulture) :> obj
        | c -> typeof<string>, c :> obj
    content




doit "2016-02-04"
