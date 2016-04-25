


// Loading test libraries

#r @"../../Ontomotor/bin/Debug/Ontomotor.dll"
#r @"../../ProtoTypeProvider/bin/Debug/ProtoTypeProvider.dll"
open MarkdownParser
open MarkdownParser.Tokenize

// Multi file test
// 
let foo = new Proto.TypeProvider.MarkdownProvider<"""C:\proj\ontomotor\src\data\test\test1\""">()
let moo = new Proto.TypeProvider.MarkdownProvider<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops.md""">() //.RootContainer.FooContainer.BarContainer()
let noo = moo.Root.Foo.Bar.GetType().GetInterfaces()



//Proto.TypeProvider.IZimbo
//type FFF = Proto.TypeProvider.ConfirmedTest
type Zzzz = Proto.TypeProvider.InterfaceImpl
//type IFff = Zzzz.IZimbo
let sss = new Zzzz()
//:> IFff
sss.Name
//let roo = new Proto.TypeProvider.PleaseFindThis()

//type Aroo =
//    member x.Hoo = 2
//
//    interface IFff 
    

foo.Documents.content_autoprops.GetType().GetInterfaces()
let x = foo.Documents.content_autoprops.Root.Foo.Bar.GetType().GetInterfaces()

let roo = foo.Documents.content_autoprops.Root.Foo.Bar
roo.RawRawr
roo.GetType().GetInterfaces()

let x1 = foo.Documents.content_autoprops.Root.Foo.Bar
let o = foo.Documents.content_autoprops.Root.Foo.Bar.Baz
let d = foo.Documents.content_autoprops.Root.Foo.GetType().GetInterfaces()
foo.Documents.content_autoprops_badstructure.Root.H1_1.H1_1_H2_1

open Proto.TypeProviderType.Provide

for d in foo.Docs do
    printfn "%s" d.Location
foo.Docs |> Seq.length

//foo.GetType().GetProperties()
//foo.GetType().GetProperty("Documents").GetValue(foo)
//foo.Documents.content_autoprops_tricky
//
//let (?) (this : 'Source) (prop : string) : 'Result =
//    let p = this.GetType().GetProperty(prop)
//    p.GetValue(this, null) :?> 'Result

foo.Documents.frontmatter_withlists // .``content-autoprops`` // .``content-autoprops-simple``.Root.Foo.Bar.BoolProp

foo.Documents.content_autoprops_badstructure.Root.H1_1.H1_1_H2_1
foo


//let docList : Proto.TypeProviderType.Provider.MarkdownFile list = [ foo.Document1; foo.Document2; foo.Document3 ]
//
//for d in docList do
//    printfn "%s" d.Location


//foo.Location
//
//foo.Document6.Document_Root.categories
//
//foo.Document1.Document_Root.H1_1
//
//foo.Document4.Document_Root.Wakka_Wakka.Long_text_that_is_made_to_cause_confusion_and_problems_with_loading_the_subproperty_names.Correct_header_content_afterwards
//foo.Document7.Document_Root.date

// Single file test 
//
//let f = @"C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md"
//let foo = new Proto.TypeProvider.MarkdownFile<"""C:\proj\ontomotor\src\data\test\test1\content-autoprops-simple.md""">()
//foo.Document_Root.Foo.Bar.DateProp
//foo.Document_Root.Foo.Bar.Prop
//foo.Document_Root.Foo.Bar.BoolProp
//foo.Document_Root.Foo.Bar.BoolProp2
//let f = foo.Document_Root.Foo.Bar //.FloatProp
//f.FloatProp
//f.FloatProp2
//f.IntProp
//f.Prop





















//
//foo.Document_Root.Boom.Wha.Autoprop1
//foo.Document_Root.Foo.SubObject.Dateprop
//foo.Document_Root.Wakka_Wakka.Long_text_that_is_made_to_cause_confusion_and_problems_with_loading_the_subproperty_names.Incorrect_header_level_with_no_content.Another_incorrect_header_with_no_content2.Title

//.Foo.SubObject.``Autoprop:_This_is_an_autoprop``

//foo.Document_Root.Foo.SubObject.``Boolprop:_true``







//foo.Document_Root.H1_2.H1_2_H3_1 //  .H1_1.H1_1_H2_1.Title
//foo.Document_Root.Foo.Bar.``Prop:_there``







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