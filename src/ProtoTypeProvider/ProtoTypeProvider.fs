
namespace Proto.TypeProviderType

open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
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

            let proxyTy = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', tyName, Some typeof<MarkdownFile>)

            proxyTy.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownFile(filename) @@>))
            proxyTy.AddMember(ProvidedConstructor(
                                [ProvidedParameter("filename", typeof<string>)],
                                InvokeCode = fun [filename] -> <@@ new MarkdownFile(%%filename) @@>))


            let domTree = filename |> Parse.file  



            let rec addPropertiesTo parentTy (Node(token, subtree):TokenTree) =
                let containerTy = ProvidedTypeDefinition(token.Title + "Container", Some typeof<MdDomEl>)

                let prop =
                    match token with
                    | Root(i,c) | Header(i,c) ->
                        ProvidedProperty(propertyName = token.Title, 
                                         propertyType = containerTy, 
                                         GetterCode = fun args -> 
                                            let title = token.Title
                                            <@@ MdDom.create (title) @@>)
                    | Property _ | Yaml _ -> 
                        ProvidedProperty(propertyName = token.Title, 
                                         propertyType = typeof<string>, 
                                         GetterCode = fun args -> 
                                            let content = token.Content
                                            <@@ content @@>)

                for node in subtree do
                    node |> addPropertiesTo containerTy 

                parentTy.AddMember prop
                parentTy.AddMember containerTy



            domTree |> addPropertiesTo proxyTy 
            
            proxyTy.AddXmlDocDelayed (fun () -> sprintf "<summary>Typed representation of an '%s' file.</summary>" Provider.proxyName)
            proxyTy
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
