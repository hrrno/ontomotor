﻿


// Loading test libraries

#r @"../../Ontomotor/bin/Debug/Ontomotor.dll"
#r @"../../ProtoTypeProvider/bin/Debug/ProtoTypeProvider.dll"


open MarkdownParser
open MarkdownParser.Tokenize

let f = @"C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md"
let foo = new Proto.TypeProvider.MarkdownFile<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md""">()
foo.FoundItem
foo.BlehBleh
foo.Filename
foo.Location
foo.Document_Root


//let g = new Proto.TypeProvider.Type1
//let o2 = new Proto.TypeProvider.MarkdownFileTypeProvider<"ha">()
//let o1 = new Proto.TypeProvider.MarkdownFileTypeProvider<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md""">()

//let z = o1.ToString()

//
//
//
//
//let obj1 = new Proto.TypeProvider.Type4("yo") // ProtoTypeProvider.ProtoTypeProvider()
//obj1.InstanceMethod


// Proto.TypeThis<"mydoc">