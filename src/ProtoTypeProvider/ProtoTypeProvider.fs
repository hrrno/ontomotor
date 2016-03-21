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


[<AutoOpen>]
module internal ActivePatterns =
    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"

module Helpers =

    let namespaceName = "Proto.TypeProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()
    

type MarkdownFile (filename) =

    member this.Filename with get () = filename
    member this.Location with get () = System.IO.Path.GetDirectoryName(filename)


type MarkdownDomElement (input) =
    let x = 1
    member this.Rawr with get () = input

module MarkdownDom =
    let create propval =
        new MarkdownDomElement(propval)

type nodeInstance =
    {
        Node : string
        InstanceId : string
        Config : string
    }

module private NodeInstance =
    let create name id config =
        { Node = name; InstanceId = id; Config = config }


[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
        
    let ns = "Proto.TypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let mdFileTypeProviderName = "MarkdownFile"
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


    


    let createMainTypes =
        // Create the main provided type
        let tyMarkdownFile = ProvidedTypeDefinition(asm, ns, mdFileTypeProviderName, Some typeof<MarkdownFile>)

        // Parameterize the type by the file to use as a template
        let filename = ProvidedStaticParameter("filename", typeof<string>)
        tyMarkdownFile.AddMembersDelayed(fun () -> createTypes "")
        tyMarkdownFile.DefineStaticParameters(
            [filename], fun tyName [| :? string as filename |] ->

            let filename' =
                if not <| System.IO.File.Exists filename then
                    let resolvedFilename = System.IO.Path.Combine(config.ResolutionFolder, filename)
                    if not <| System.IO.File.Exists resolvedFilename then
                        failwithf "File '%s' not found" resolvedFilename
                    resolvedFilename
                else
                    filename 

            let ty = ProvidedTypeDefinition(asm, ns, tyName, Some typeof<MarkdownFile>)
            ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownFile(filename') @@>))
            ty.AddMember(ProvidedConstructor(
                            [ProvidedParameter("filename", typeof<string>)],
                            InvokeCode = fun [filename] -> <@@ new MarkdownFile(%%filename) @@>))



            // add child prop with value or two...
            let tree = Parse.file filename' 
            let container = ProvidedTypeDefinition("treeContainer", Some typeof<MarkdownDomElement>)
            let membbrrr = ProvidedProperty(propertyName = "treeeee", 
                                            propertyType = container, 
                                            GetterCode = fun args -> <@@ MarkdownDom.create "onetwothree" @@>) //(fun (Singleton doc) -> doc))
            ty.AddMember(membbrrr)
            ty.AddMember container

            //let subRow = ProvidedTypeDefinition("ZRow", Some typeof<string>)

            //let create, subtype = create table type: tuple of creation function and type

            let subtype = ProvidedTypeDefinition("subtreeprop", Some typeof<string>)
//
//            let rowConverter =
//                let rowVar = Var("row", typeof<string[]>)
//                let rowVarExpr = Expr.Var rowVar
//                let body =
//                  if fields.Length = 1
//                  then fields.Head.Convert rowVarExpr
//                  else Expr.NewTuple [ for field in fields -> field.Convert rowVarExpr ]
//        
//                let delegateType = 
//                  typedefof<Func<_,_>>.MakeGenericType(typeof<string[]>, rowErasedType)
//        
//                Expr.NewDelegate(delegateType, [rowVar], body)
//
//            let create (htmlDoc:Expr) =
//                let rowConverterVar = Var("rowConverter", rowConverter.Type)
//                let body = tableErasedWithRowErasedType?Create () (Expr.Var rowConverterVar, htmlDoc, table.Name, table.HasHeaders.Value)
//
//                let rawr = "herrrooooooo"
//                Expr.Let(rowConverterVar, rowConverter, body)

            ty.AddMember(subtype)
            let subTableProp = ProvidedProperty("subtreee", subtype, GetterCode = (fun _ -> Expr.Value "fwaaaaa")) // (Singleton doc) -> create doc))
            container.AddMember(subTableProp)

//
//            let addInputPort (inputs : ProvidedTypeDefinition) (port : Port) =
//                let port = ProvidedProperty(
//                                port.Id.Name,
//                                typeof<InputPort>,
//                                GetterCode = fun args ->
//                                    let id = port.Id.UniqueId.ToString()
//                                    <@@ GetPort id @@>)
//                inputs.AddMember(port)
//
            let addOutputPort (outputs : ProvidedTypeDefinition) (name: string) =
                let port = ProvidedProperty(
                                name,
                                typeof<string>,
                                GetterCode = fun args ->
                                    let id = "ahoy" // port.Id.UniqueId.ToString()
                                    <@@ id @@>)
                outputs.AddMember(port)
//
//            let addPorts inputs outputs (portList : seq<Port>) =
//                portList
//                |> Seq.iter (fun port ->
//                                match port.Type with
//                                | "input" -> addInputPort inputs port
//                                | "output" -> addOutputPort outputs port
//                                | _ -> failwithf "Unknown port type for port %s/%s" port.Id.Name (port.Id.UniqueId.ToString()))

            let createNodeType name =
                let nodeType = ProvidedTypeDefinition(asm, ns, name, Some typeof<nodeInstance>)
                let ctor = ProvidedConstructor(
                            [
                                ProvidedParameter("Name", typeof<string>)
                                ProvidedParameter("UniqueId", typeof<string>)
                                ProvidedParameter("Config", typeof<string>)
                            ],
                            InvokeCode = fun [name;id;config] -> <@@ NodeInstance.create (%%name:string) (%%id:string) (%%config:string) @@>)
                nodeType.AddMember(ctor)

                let outputs = ProvidedTypeDefinition("Outputs", Some typeof<obj>)
                let outputCtor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
                outputs.AddMember(outputCtor)
                outputs.HideObjectMethods <- true

                addOutputPort outputs "zzzzom"

//                addPorts inputs outputs node.Ports

                // Add the inputs and outputs types of nested types under the Node type
                nodeType.AddMembers([outputs])

                // Now add some instance properties to expose them on a node instance.
                let outputPorts = ProvidedProperty("OutputPorts", outputs, [],GetterCode = fun args -> <@@ obj() @@>)

                nodeType.AddMembers([outputPorts])

                nodeType



            //ty.AddMember(createNodeType "hey")
                            
            ty.AddXmlDocDelayed (fun () -> sprintf "This is a very nice provided type '%s'" mdFileTypeProviderName)
            ty
            )
        [ tyMarkdownFile ]

    do this.AddNamespace(Helpers.namespaceName, createMainTypes)

                            
[<assembly:TypeProviderAssembly>] 
do()







//            let rec propGen (parentTy:ProvidedTypeDefinition) (Node(node,sub)) =
//                let newTy = ProvidedTypeDefinition("NestedType_" + node.Title, Some typeof<obj>, HideObjectMethods = true, IsErased = false)
//                //newTy.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new System.Object() @@>))
//                
//                let title = node.Title
//                let memberProp = 
//                    ProvidedProperty(propertyName = "Title", 
//                                     propertyType = typeof<string>, 
//                                     //IsStatic = true,
//                                     GetterCode = (fun args -> <@@ title @@>))
//
//                newTy.AddMember(memberProp)
//                for n in sub do propGen newTy n
//                
//                parentTy.AddMemberDelayed(fun () -> newTy)
//
//            let tree = Parse.file filename' 
//            tree |> propGen ty







//                let subProp = 
//                    ProvidedProperty(propertyName = node.Title, 
//                                     propertyType = typeof<obj>, 
//                                     IsStatic = true,
//                                     GetterCode = (fun args -> <@@ newTy @@>))


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