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
open System.IO

// list MD files in a folder

let filename = @"C:\proj\ontomotor\src\data\test\test1"
let isDir dir = Directory.Exists(dir)

isDir filename

let files = System.IO.Directory.GetFiles(filename, "*.md", IO.SearchOption.AllDirectories)
