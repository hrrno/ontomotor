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


type MdDomEl =
    { Title: string; }

module MdDom =
    let create name =
        { Title = name } 



[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
        
    let ns = "Proto.TypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    let mdFileTypeProviderName = "MarkdownFile"


    let createMainTypes =
        let tyMarkdownFile = ProvidedTypeDefinition(asm, ns, mdFileTypeProviderName, Some typeof<MarkdownFile>)
        let filename = ProvidedStaticParameter("filename", typeof<string>)

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
            let tree = Parse.file filename' //|> Seq.head



            let rec treeProp containerTy (item:TokenTree) =
                let itemsContainer = ProvidedTypeDefinition(item.Token.Title + "Container", Some typeof<MdDomEl>)
                let itemsMember = ProvidedProperty(propertyName = item.Token.Title, 
                                                propertyType = itemsContainer, 
                                                GetterCode = fun args -> 
                                                    let foo = item.Token.Title
                                                    <@@ MdDom.create (foo) @@>) //(fun (Singleton doc) -> doc))

                for node in item.Sub do
                    treeProp itemsContainer node

                containerTy.AddMember itemsMember
                containerTy.AddMember itemsContainer



            treeProp ty tree
//
//
//
//            let container = ProvidedTypeDefinition("treeContainer", Some typeof<MarkdownDomElement>)
//            let membbrrr = ProvidedProperty(propertyName = "treeeee", 
//                                            propertyType = container, 
//                                            GetterCode = fun args -> <@@ MarkdownDom.create "onetwothree" @@>) //(fun (Singleton doc) -> doc))
//            ty.AddMember(membbrrr)
//            ty.AddMember container
//
//
//            let subtype = ProvidedTypeDefinition("subtreeprop", Some typeof<string>)
//            ty.AddMember(subtype)
//            let subTableProp = ProvidedProperty("subtreee", subtype, GetterCode = (fun _ -> Expr.Value "fwaaaaa")) // (Singleton doc) -> create doc))
//            container.AddMember(subTableProp)

            
                            
            ty.AddXmlDocDelayed (fun () -> sprintf "<summary>Typed representation of an '%s' file.</summary>" mdFileTypeProviderName)
            ty
            )
        [ tyMarkdownFile ]
    
    do this.AddNamespace(Helpers.namespaceName, createMainTypes)

                            
[<assembly:TypeProviderAssembly>] 
do()



//    let createNodeType name =
//        let nodeType = ProvidedTypeDefinition(name, Some typeof<nodeInstance>) /// somewhere in here I`m getting errors, need to send in parent type?
//                                                                                        // might just have to delete the namespace and let it go in as `simple`...
//        let ctor = ProvidedConstructor(
//                    [
//                        ProvidedParameter("Name", typeof<string>)
//                        ProvidedParameter("UniqueId", typeof<string>)
//                        ProvidedParameter("Config", typeof<string>)
//                    ],
//                    InvokeCode = fun [name;id;config] -> <@@ NodeInstance.create (%%name:string) (%%id:string) (%%config:string) @@>)
//        nodeType.AddMember(ctor)



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





//    let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
//
//    let rec createTypes wordSoFar =
//        alphabet
//        |> Seq.map (fun t -> let newChar = t |> string
//                             let newWord = wordSoFar + newChar
//                             let ty = ProvidedTypeDefinition(newChar, None)
//                             ty.AddMembersDelayed(fun () -> createTypes newWord)
//                             let wordProp = ProvidedProperty("Word", typeof<string>, IsStatic=true, GetterCode = fun args -> <@@ newWord @@>)
//                             ty.AddMember(wordProp)
//                             ty)
//        |> Seq.toList
