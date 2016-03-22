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
module internal ActivePatterns =
    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"

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

type MdDomEl =
    { Title: string; }

module MdDom =
    let create name =
        { Title = name } 

module Provide =

    let proxyType typeName =
        ProvidedTypeDefinition(Provider.assembly, Provider.namespace', 
                               typeName, Some typeof<MarkdownFile>)

    let markdownProxy filename generatedTypeName = 
        let proxyType = proxyType generatedTypeName
        proxyType.AddMember(ProvidedConstructor([], InvokeCode = fun [] -> <@@ new MarkdownFile(filename) @@>))
        proxyType.AddMember(ProvidedConstructor(
                            [ProvidedParameter("filename", typeof<string>)],
                            InvokeCode = fun [filename] -> <@@ new MarkdownFile(%%filename) @@>))
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
                (containerType :> Type), <@@ MdDom.create title @@>
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
        let containerTy = ProvidedTypeDefinition(token.Title + "Container", Some typeof<MdDomEl>)
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

            let filename = Provider.resolveFilename markdownPath config.ResolutionFolder
            let proxyType = Provide.markdownProxy filename generatedTypeName

            filename 
            |> Parse.file  
            |> Provide.properties proxyType 
            
            //proxyType.AddXmlDocDelayed (fun () -> sprintf "<summary>Typed representation of an '%s' file.</summary>" Provider.proxyName)
            proxyType
        )
        proxyRoot
    
    do this.AddNamespace(Provider.namespace', [ createProxy ])

[<assembly:TypeProviderAssembly>] 
do()
