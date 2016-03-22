


// Loading test libraries

#r @"../../Ontomotor/bin/Debug/Ontomotor.dll"
#r @"../../ProtoTypeProvider/bin/Debug/ProtoTypeProvider.dll"


open MarkdownParser
open MarkdownParser.Tokenize

let f = @"C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md"
let foo = new Proto.TypeProvider.MarkdownFile<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md""">()
foo.Document_Root.Foo.Bar.DateProp
foo.Document_Root.Foo.Bar.Prop
foo.Document_Root.Foo.Bar.BoolProp
foo.Document_Root.Foo.Bar.BoolProp2





//
//foo.Document_Root.Boom.Wha.Autoprop1
//foo.Document_Root.Foo.SubObject.Dateprop
//foo.Document_Root.Wakka_Wakka.Long_text_that_is_made_to_cause_confusion_and_problems_with_loading_the_subproperty_names.Incorrect_header_level_with_no_content.Another_incorrect_header_with_no_content2.Title

//.Foo.SubObject.``Autoprop:_This_is_an_autoprop``

//foo.Document_Root.Foo.SubObject.``Boolprop:_true``







//foo.Document_Root.H1_2.H1_2_H3_1 //  .H1_1.H1_1_H2_1.Title
//foo.Document_Root.Foo.Bar.``Prop:_there``





foo.Filename
foo.Location



//Proto.TypeProvider.StartHere.A.B.C.Word
//Proto.TypeProvider.MarkdownFile.H.A.B.E.A
//let bar = Proto.TypeProvider.MarkdownFile<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md""">
//let s = new Proto.TypeProvider.MarkdownFile
//foo.FoundItem
//foo.BlehBleh


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