﻿// Copyright (c) Microsoft Corporation 2005-2011.
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

namespace ProtoType

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations


module Helpers =

    let namespaceName = "Proto.TypeProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()


module BasicTypeMaker = 


    // Make one provided type, called TypeN
    let makeOneProvidedType (n:int) = 

        // This is the provided type. It is an erased provided type, and in compiled code 
        // will appear as type 'obj'.
        let t = ProvidedTypeDefinition(Helpers.thisAssembly,Helpers.namespaceName,
                                       "Type" + string n,
                                       baseType = Some typeof<obj>)

        // Add documentation to the provided type.
        t.AddXmlDocDelayed (fun () -> sprintf "This provided type %s" ("Type" + string n))
        
        // This is a provided static property. A get of this property will always evaluate to 
        // the string "Hello!".
        //
        // The GetterCode for the property returns an F# quotation. This represents the code 
        // generated by the host compiler for a get of the property. 
        let staticProp = ProvidedProperty(propertyName = "StaticProperty", 
                                          propertyType = typeof<string>, 
                                          IsStatic=true,
                                          GetterCode= (fun args -> <@@ "Hello!" @@>))

        // Add documentation to the provided static property.
        staticProp.AddXmlDocDelayed(fun () -> "This is a static property")

        // Add the static property to the type.
        t.AddMember staticProp

        // This is provided constructor. The constructor takes no parameters. 
        // 
        // For example, a use of this contructor: 
        //     new Type10()
        // will create an instance of the provided type with underlying data "The object data".
        //
        // In this sample, the provided type is erased to type ‘obj’ and all uses of the type
        // will appear as type ‘obj’ in compiled code. In these examples the underlying
        // objects are in fact strings. 
        //
        // As with all uses of type erasure, expliciting boxing, unboxing 
        // and casting can be used to subvert erased types. In this case, an
        // invalid cast exception may result when the object is used. A provider 
        // runtime can define its own private representation type to help protect 
        // against false representations. 
        //
        // The InvokeCode for the constructor returns an F# quotation. This represents the code 
        // generated by the host compiler for a get of the property. 
        let ctor = ProvidedConstructor(parameters = [ ], 
                                       InvokeCode= (fun args -> <@@ "The object data" :> obj @@>))

        // Add documentation to the provided constructor.
        ctor.AddXmlDocDelayed(fun () -> "This is a constructor")

        // Add the provided constructor to the provided type.
        t.AddMember ctor

        // This is a provided constructor with one parameter.
        //
        // For example, a use of this contructor: 
        //     new Type10("ten")
        // will create an instance of the provided type with underlying data "ten".
        //
        // The InvokeCode for the constructor returns an F# quotation. This represents 
        // the code generated by the host compiler for a call to the method. InvokeCode is a 
        // function returning a quotation. An expression representing the parameter value 
        // is in args.[0]. The code for a call to the constructor coerces
        // args.[0] to the erased type 'obj'. 
        let ctor2 = 
            ProvidedConstructor(parameters = [ ProvidedParameter("data",typeof<string>) ], 
                                InvokeCode= (fun args -> <@@ (%%(args.[0]) : string) :> obj @@>))

        ctor2.AddXmlDocDelayed(fun () -> "This is a constructor")
        // Add the constructor to the type.
        t.AddMember ctor2

        // This is a provided instance property. A get of this property will evaluate to 
        // the length of the string which is the representation object.
        //
        // Note the GetterCode, which returns an F# quotation giving the code generated by the
        // host compiler for a get of the property. GetterCode is a function
        // returning a quotation – the host compiler calls this function with
        // an expression representing the instance object is supplied as args.[0]. 
        // The implementation of GetterCode then splices into the result quotation 
        // at the erased type 'obj',and a cast used to 'prove' that the object is 
        // a string. 
        let instanceProp = 
            ProvidedProperty(propertyName = "InstanceProperty", 
                             propertyType = typeof<int>, 
                             GetterCode= (fun args -> 
                                             <@@ ((%%(args.[0]) : obj) :?> string).Length @@>))

        instanceProp.AddXmlDocDelayed(fun () -> "This is an instance property")
        // Add the instance property to the type.
        t.AddMember instanceProp 


        // This is an instance method with one parameter. A call to the method will 
        // evaluate to the character in the representation object at the given index.
        //
        // Note the InvokeCode, which returns an F# quotation giving the code generated by the
        // host compiler for a call to the method. InvokeCode is a function
        // returning a quotation – the host compiler calls this function with
        // an expression representing the instance object is supplied as args.[0]. 
        // The implementation of GetterCode then splices into the result quotation 
        // at the erased type 'obj',and a cast used to 'prove' that the object is 
        // a string. 
        //
        // An expression representing the parameter value is available in args.[1].
        let instanceMeth = 
            ProvidedMethod(methodName = "InstanceMethod", 
                           parameters = [ProvidedParameter("x",typeof<int>)], 
                           returnType = typeof<char>, 
                           InvokeCode = (fun args -> 
                              <@@ ((%%(args.[0]) : obj) :?> string).Chars(%%(args.[1]) : int) @@>))

        instanceMeth.AddXmlDocDelayed(fun () -> "This is an instance method")
        // Add the instance method to the type.
        t.AddMember instanceMeth 

        // This is a nested type. It is provided on-demand. In compiled code it will 
        // appear as type 'obj'.
        t.AddMembersDelayed(fun () -> 
            let nestedType = ProvidedTypeDefinition("NestedType",
                                                    Some typeof<obj>)

            // Each nested type contains 100 static properties, provided on-demand.
            // The static properties have constant values.
            nestedType.AddMembersDelayed (fun () -> 
                let staticPropsInNestedType = 
                    [ for i in 1 .. 100 do
                        let valueOfTheProperty = "I am string "  + string i

                        let p = ProvidedProperty(propertyName = "StaticProperty" + string i, 
                                                 propertyType = typeof<string>, 
                                                 IsStatic=true,
                                                 GetterCode= (fun args -> <@@ valueOfTheProperty @@>))

                        p.AddXmlDocDelayed(fun () -> 
                               sprintf "This is StaticProperty%d on NestedType" i)

                        yield p ]
                
                staticPropsInNestedType)

            [nestedType])

        // The result is the type.
        t



type MarkdownFile (filename) =
    let name = filename
    let Hey = "yo"


// This defines the type provider. When compiled to a DLL it can be added as a reference to an F#
// command-line compilation, script or project.
[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 

    // Inheriting from this type provides implementations of ITypeProvider in terms of the
    // provided types below.
    inherit TypeProviderForNamespaces()
//
//    let markdownProvTy = ProvidedTypeDefinition(Helpers.thisAssembly, Helpers.namespaceName,
//                                "MarkdownFile",
//                                baseType = Some typeof<obj>, HideObjectMethods = true)
//
//
//    let buildTypes (typeName:string) (args:obj[]) =
//
//        let path = args.[0] :?> string
//        
//        if path = "" then failwith "The file path parameter cannot be empty"
//
//
//
//    // Add static parameter that specifies the API we want to get (compile-time) 
//    let parameters = 
//        [ ProvidedStaticParameter("FilePath", typeof<string>, parameterDefaultValue = "") ]
//
//    let helpText = 
//        """<summary>A typed representation of a Markdown file.</summary>
//            <param name='FilePath'>Location of the markdown file to create.</param>"""


    //do markdownProvTy.AddXmlDoc helpText
    //do markdownProvTy.DefineStaticParameters (parameters, buildTypes)
    
//


    // this is working just fine...

    let types = [ for i in 1 .. 4 -> BasicTypeMaker.makeOneProvidedType i ] 
    do this.AddNamespace(Helpers.namespaceName, types)


    // this is throwing object not found for whatever reason...

    
    let ns = "Proto.TypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let mdFileTypeProviderName = "MarkdownFileTypeProvider"
    let mdLibraryTypeProviderName = "MarkdownLibraryTypeProvider"

    let createTypes () =
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

    do this.AddNamespace(ns, createTypes())

                            
[<assembly:TypeProviderAssembly>] 
do()


