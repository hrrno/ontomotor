namespace Proto.TypeProviderType

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open MarkdownParser
open MarkdownParser.Tokenize

[<AutoOpen>]
module internal Utility =
    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"
        
    let filename path = Path.GetFileName(path)
    let markdownFiles path = Directory.GetFiles(path, "*.md", IO.SearchOption.AllDirectories)
    let isDir path = Directory.Exists(path)
    let isFile path = File.Exists(path)

    let (|IsDir|_|) path = if isDir path then Some path else None
    let (|IsFile|_|) path = if isFile path then Some path else None
    let (|Exists|_|) = function
        | IsDir path | IsFile path -> Some path 
        | _ -> None

    let isMatch regex input = Regex(regex, RegexOptions.IgnoreCase).IsMatch(input)
    let checkRegex regex input = if isMatch regex input then Some input else None
    let (|IsMatch|_|) regex input = checkRegex regex input
    let (|IsDate|_|)   input = input |> checkRegex "(\d{1,4})-(\d{1,2})-(\d{1,2})"
    let (|IsBool|_|)   input = input |> checkRegex "(true|false)"
    let (|IsInt|_|)    input = input |> checkRegex "(\d{1,9})"
    let (|IsDouble|_|) input = input |> checkRegex "(\d{1,15})(,|\.)(\d{1,8})"

module Provider =

    let namespace' = "Proto.TypeProvider"
    let assembly = Assembly.GetExecutingAssembly()
    let proxyName = "MarkdownProvider"
    
    let resolvePath resolutionFolder path = 
        match path with
        | Exists path -> path
        | _ -> 
            let relative = Path.Combine(resolutionFolder, path)
            match relative with
            | Exists relative -> relative
            | _ -> failwithf "File or directory '%s' (relative path '%s') not found" path relative

    type Source =
        | SingleFile 
        | MultiFile

    let mode path =
        match path with
        | IsDir path -> MultiFile 
        | _ -> SingleFile 


type MarkdownSource (path) =

    member this.Source with get () = path

type MarkdownFile (filename) =

    static member safeName file = Path.GetFileNameWithoutExtension(file)
                                      .Replace(" ", "_")
                                      .Replace(".md", "")
                                      .Replace("-", "_")

    member this.Filename with get () = filename
    member this.Location with get () = Path.GetDirectoryName(filename)

type MarkdownElement =
    { Title: string; }

module MarkdownDom =
    let create name =
        { Title = name } 

module Provide =

    let proxyType typeName =
        ProvidedTypeDefinition(Provider.assembly, Provider.namespace', 
                               typeName, Some typeof<MarkdownSource>)

    let markdownProxy filename generatedTypeName = 
        let proxyType = proxyType generatedTypeName
        proxyType.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownSource(filename) @@>))
        proxyType.AddMember(ProvidedConstructor(
                            [ProvidedParameter("filename", typeof<string>)],
                            InvokeCode = fun [filename] -> <@@ new MarkdownSource(%%filename) @@>))
        proxyType

    let date str = DateTime.Parse(str)
    let bool str = bool.Parse(str)
    let string str = str
    let int str = Int32.Parse(str)
    let float str = 
        let sep = CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
        Double.Parse((str:string).Replace(".", sep).Replace(",", sep))

    let propFor containerType token =
        let (type', getter:Quotations.Expr) =
            match token with
            | Root(i,c) | Header(i,c) ->
                let title = token.Title
                (containerType :> Type), <@@ MarkdownDom.create title @@>
            | Property _ | Yaml _ -> 
                match token.Content with
                | IsBool c   -> typeof<bool>,     <@@ bool c @@> 
                | IsDate c   -> typeof<DateTime>, <@@ date c @@>
                | IsDouble c -> typeof<float>,    <@@ float c @@> 
                | IsInt c    -> typeof<int>,      <@@ int c @@> 
                | c          -> typeof<string>,   <@@ string c @@> 

        ProvidedProperty(propertyName = token.Title, 
                         propertyType = type', 
                         GetterCode = fun args -> getter)

    let rec properties parentTy (Node(token, subtree):TokenTree) =
        let containerTy = ProvidedTypeDefinition(token.Title + "Container", Some typeof<MarkdownElement>)
        let prop = token |> propFor containerTy 

        for node in subtree do 
            node |> properties containerTy 

        parentTy.AddMember prop
        parentTy.AddMember containerTy
        

[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
        
    let createProxy =
        let proxyRoot = Provide.proxyType Provider.proxyName

        proxyRoot.DefineStaticParameters(
            [ ProvidedStaticParameter("path", typeof<string>) ], 
            fun generatedTypeName [| :? string as markdownPath |] ->

            let source = Provider.resolvePath config.ResolutionFolder markdownPath
            let proxyType = Provide.markdownProxy source generatedTypeName

            match Provider.mode markdownPath with
            | Provider.SingleFile ->
                source 
                |> Parse.file  
                |> Provide.properties proxyType 
                
            | Provider.MultiFile -> 

                let docCollectionType = ProvidedTypeDefinition("MarkdownSequence", Some typeof<obj>)

                // translate this function into something that returns a sequence of docTypes and parse trees
                // Add them to the docs property, as well as into a collection

                for file in source |> markdownFiles do
                    let docType = ProvidedTypeDefinition("DocumentContainer" + (file |> MarkdownFile.safeName), 
                                                         Some typeof<MarkdownFile>)
                    let docProp = ProvidedProperty(propertyName = (file |> MarkdownFile.safeName), 
                                                   propertyType = docType, 
                                                   GetterCode = fun args -> <@@ new MarkdownFile(file) @@>)
                    file
                    |> Parse.file  
                    |> Provide.properties docType 

                    docCollectionType.AddMember docProp
                    proxyType.AddMember docType

                proxyType.AddMember(ProvidedProperty(
                                        "Docs", docCollectionType,
                                        GetterCode = fun args -> <@@ new obj() @@>))

                proxyType.AddMember docCollectionType

                ()
            proxyType
        )
        proxyRoot
    
    do this.AddNamespace(Provider.namespace', [ createProxy ])

[<assembly:TypeProviderAssembly>] 
do()



// want a collection proxy
    // takes a folder
    // provides a list of `documents`
    // Loop current proxy creation on a list of docs...

        // folder -> list of files -> list of trees -> list of proxies with trees filled out



// Add support for relative directories





//                sasFile.MetaData.Columns
//                |> Seq.map (fun col ->
//                    let i = col.Ordinal - 1 
//                    ProvidedProperty(col.Name, typeof<Value>,
//                        GetterCode = fun [values] ->
//                                        <@@ (Seq.nth i (%%values: Value seq) ) @@> ) 
//                    )
//                |> Seq.toList
//                |> tyObservation.AddMembers