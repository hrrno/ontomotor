// Copyright (c) Microsoft Corporation 2005-2011.
// This sample code is provided "as is" without warranty of any kind. 
// We disclaim all warranties, either express or implied, including the 
// warranties of merchantability and fitness for a particular purpose. 

// This is a sample type provider. It provides 100 types, each containing various properties, 
// methods and nested types.
//
// This code is a sample for use in conjunction with the F# 3.0 Developer Preview release of September 2011.
//
// 1. Using the Provider
// 
//   To use this provider, open a separate instance of Visual Studio 11 and reference the provider
//   using #r, e.g.
//      #r @"bin\Debug\HelloWorldTypeProvider.dll"
//
//   Then look for the types under 
//      Samples.HelloWorldTypeProvider
//
// 2. Recompiling the Provider
//
//   Make sure you have exited all instances of Visual Studio and F# Interactive using the 
//   provider DLL before recompiling the provider.
//
// 3. Debugging the Provider
//
//   To debug this provider using 'print' statements, make a script that exposes a 
//   problem with the provider, then use
// 
//      fsc.exe -r:bin\Debug\HelloWorldTypeProvider.dll script.fsx
//
//   To debug this provider using Visual Studio, use
//
//      devenv.exe /debugexe fsc.exe -r:bin\Debug\HelloWorldTypeProvider.dll script.fsx
//
//   and disable "Just My Code" debugging. Consider setting first-chance exception catching using 
//
//      Debug --> Exceptions --> CLR Exceptions --> Thrown

namespace Proto.TypeProviderType

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Proto.TypeProvider
open MarkdownParser
open MarkdownParser.Tokenize

module Helpers =

    let namespaceName = "Proto.TypeProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    

type MarkdownFile (filename) =

    member this.Filename with get () = filename
    member this.Location with get () = System.IO.Path.GetDirectoryName(filename)





[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
        
    let ns = "Proto.TypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let mdFileTypeProviderName = "MarkdownFile"


    let createMainTypes () =
        // Create the main provided type
        let tyMarkdownFile = ProvidedTypeDefinition(asm, ns, mdFileTypeProviderName, None)

        // Parameterize the type by the file to use as a template
        let filename = ProvidedStaticParameter("filename", typeof<string>)

        tyMarkdownFile.DefineStaticParameters(
            [filename], fun tyName [| :? string as filename |] ->

            let filename' =
                if not <| System.IO.File.Exists filename then
                    // resolve the filename relative to the resolution folder
                    let resolvedFilename = System.IO.Path.Combine(config.ResolutionFolder, filename)
                    if not <| System.IO.File.Exists resolvedFilename then
                        failwithf "File '%s' not found" resolvedFilename
                    resolvedFilename
                else
                    filename 

            // define the provided type, erasing to SasFile
            let ty = ProvidedTypeDefinition(asm, ns, tyName, Some typeof<MarkdownFile>)

            // add a parameterless constructor which loads the file that was used to define the schema
            ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownFile(filename') @@>))

            // add a constructor taking the filename to load
            ty.AddMember(ProvidedConstructor(
                            [ProvidedParameter("filename", typeof<string>)],
                            InvokeCode = fun [filename] -> <@@ new MarkdownFile(%%filename) @@>))

                            
            let rec propGen (parentTy:ProvidedTypeDefinition) (Node(node,sub)) =
                let newTy = ProvidedTypeDefinition("NestedType_" + node.Title, Some typeof<obj>, HideObjectMethods = true, IsErased = false)
                //newTy.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new System.Object() @@>))
                
                let title = node.Title
                let memberProp = 
                    ProvidedProperty(propertyName = "Title", 
                                     propertyType = typeof<string>, 
                                     IsStatic = true,
                                     GetterCode = (fun args -> <@@ title @@>))

                newTy.AddMember(memberProp)
                
                //newTy.AddMembers([ 
                
                for n in sub do 
                    propGen newTy n //])
                
//                let subProp = 
//                    ProvidedProperty(propertyName = node.Title, 
//                                     propertyType = typeof<obj>, 
//                                     IsStatic = true,
//                                     GetterCode = (fun args -> <@@ newTy @@>))

                parentTy.AddMemberDelayed(fun () -> newTy)
                //parentTy.AddMemberDelayed ()  //Delayed(fun () -> subProp)

            let tree = Parse.file filename' 
            tree |> propGen ty


            


//            let title = tree.Token.Title

//            let instanceProp = 
//                ProvidedProperty(propertyName = "BlehBleh", 
//                                 propertyType = typeof<string>, 
//                                 GetterCode = (fun args -> 
//                                                 <@@ filename' @@>))
//
//
//            let instanceProp2 = 
//                ProvidedProperty(propertyName = "FoundItem", 
//                                 propertyType = typeof<string>, 
//                                 GetterCode = (fun args -> 
//                                                 <@@ title @@>))

//            instanceProp.AddXmlDocDelayed(fun () -> "This is a generated instance property")
//            // Add the instance property to the type.
//            ty.AddMember instanceProp 
//            ty.AddMember instanceProp2 




            // Loop through and recursively add members and such for each treenode


//            // define a provided type for each row, erasing to a Value seq
//            let tyObservation = ProvidedTypeDefinition("Observation", Some typeof<Value seq>)
//       
//            // read SAS schema
//            use sasFile = new SasFile(filename')
//
//            // add one property per SAS variable
//            sasFile.MetaData.Columns
//            |> Seq.map (fun col ->
//                let i = col.Ordinal - 1 
//                ProvidedProperty(col.Name, typeof<Value>,
//                    GetterCode = fun [values] ->
//                                    <@@ (Seq.nth i (%%values: Value seq) ) @@> ) 
//                )
//            |> Seq.toList
//            |> tyObservation.AddMembers
//
//            // add a new, more strongly typed Data property (which uses the existing property at runtime)
//            ty.AddMember(ProvidedProperty(
//                            "Observations", typedefof<seq<_>>.MakeGenericType(tyObservation),
//                            GetterCode = fun [sasFile] -> <@@ (%%sasFile: SasFile).Rows @@>))
//
//            // add the row type as a nested type
//            ty.AddMember tyObservation


            ty.AddXmlDocDelayed (fun () -> sprintf "Provided type '%s'" mdFileTypeProviderName)
            ty
            )
        [ tyMarkdownFile ]


    //let baseType = Some typeof<obj>
    //let ns = "Word.TypeProvider"
    //let asm = Assembly.GetExecutingAssembly()
    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    let rec createTypes wordSoFar =
        alphabet
        |> Seq.map (fun t -> let newChar = t |> string
                             let newWord = wordSoFar + newChar
                             let ty = ProvidedTypeDefinition(newChar, None)
                             ty.AddMembersDelayed(fun () -> createTypes newWord)
                             let wordProp = ProvidedProperty("Word", typeof<string>, IsStatic=true, GetterCode = fun args -> <@@ newWord @@>)
                             ty.AddMember(wordProp)
                             ty)
        |> Seq.toList

    let rootType = ProvidedTypeDefinition(asm, ns, "StartHere", None)
    do rootType.AddMembersDelayed(fun () -> createTypes "")
    do this.AddNamespace(ns, [rootType])

    do this.AddNamespace(Helpers.namespaceName, createMainTypes())

                            
[<assembly:TypeProviderAssembly>] 
do()


