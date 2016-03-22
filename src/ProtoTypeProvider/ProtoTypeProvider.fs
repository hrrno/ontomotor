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
open System.IO


[<AutoOpen>]
module internal ActivePatterns =
    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"

module Provider =

    let namespace' = "Proto.TypeProvider"
    let assembly = Assembly.GetExecutingAssembly()
    let proxyName = "MarkdownFile"
    
    let resolveFilename path resolutionFolder = 
        if not <| File.Exists path then
            let relative = Path.Combine(resolutionFolder, path)
            if not <| File.Exists relative then
                failwithf "File '%s' not found" relative
            relative
        else
            path 

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
        

    let createProxy =
        let tyMarkdownFile = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', Provider.proxyName, Some typeof<MarkdownFile>)
        let pathParam = ProvidedStaticParameter("path", typeof<string>)

        tyMarkdownFile.DefineStaticParameters(
            [ pathParam ], fun tyName [| :? string as markdownPath |] ->

            let filename = Provider.resolveFilename markdownPath config.ResolutionFolder

            let ty = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', tyName, Some typeof<MarkdownFile>)

            ty.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownFile(filename) @@>))
            ty.AddMember(ProvidedConstructor(
                            [ProvidedParameter("filename", typeof<string>)],
                            InvokeCode = fun [filename] -> <@@ new MarkdownFile(%%filename) @@>))


            let domTree = filename |> Parse.file  



            let rec addPropertiesTo parentTy (item:TokenTree) =
                let containerTy = ProvidedTypeDefinition(item.Token.Title + "Container", Some typeof<MdDomEl>)
                let nodeProp = ProvidedProperty(propertyName = item.Token.Title, 
                                                propertyType = containerTy, 
                                                GetterCode = fun args -> 
                                                    let foo = item.Token.Title
                                                    <@@ MdDom.create (foo) @@>) //(fun (Singleton doc) -> doc))

                // need to differentiate properties and sub objects (right?)


                for node in item.Sub do
                    node |> addPropertiesTo containerTy 

                parentTy.AddMember nodeProp
                parentTy.AddMember containerTy



            domTree |> addPropertiesTo ty 
            
            ty.AddXmlDocDelayed (fun () -> sprintf "<summary>Typed representation of an '%s' file.</summary>" mdFileTypeProviderName)
            ty
            )
        [ tyMarkdownFile ]
    
    do this.AddNamespace(Provider.namespace', createProxy)

                            
[<assembly:TypeProviderAssembly>] 
do()




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
