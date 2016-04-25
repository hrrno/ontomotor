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
open MarkdownStructure
open MarkdownStructure.Interface

[<AutoOpen>]
module internal Utility =
    let (|Singleton|) = function [l] -> l | _ -> failwith "Parameter mismatch"
        
    let filename path = Path.GetFileName(path)
    let markdownFiles path = Directory.GetFiles(path, "*.md", IO.SearchOption.AllDirectories) |> Array.toList
    let isDir path = Directory.Exists(path)
    let isFile path = File.Exists(path)

    let (|IsDir|_|)  path = if path |> isDir then Some path else None
    let (|IsFile|_|) path = if path |> isFile then Some path else None
    let (|Exists|_|) = function
        | IsDir path | IsFile path -> Some path 
        | _ -> None

    let (?) (this : 'Source) (prop : string) : 'Result =
        let p = this.GetType().GetProperty(prop)
        p.GetValue(this, null) :?> 'Result


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

    type MarkdownFile (filename) =

        static member safeName file = Path.GetFileNameWithoutExtension(file)
                                          .Replace(" ", "_")
                                          .Replace(".md", "")
                                          .Replace("-", "_")

        member this.Filename with get () = filename
        member this.Location with get () = Path.GetDirectoryName(filename)
        

    type MarkdownTesting () =
        member x.Name with get () = "abctest"

    type MarkdownSource (path) =
        member x.Source with get () = path
        member x.Mode with private get () = path |> mode
        member x.Documents with get () = []
        member x.Files with get () = 
                        match x.Mode with
                        | MultiFile -> 
                            (lazy ( 
                                    path 
                                    |> markdownFiles 
                                    |> List.map(fun p -> new MarkdownFile(p)) )
                            ).Value
                        | SingleFile -> []

    type ITesting = 
        interface end    

    type ITestVal = 
        abstract member Zzz : int

    type IHaveTitle = 
        abstract member Title : string

    type TTTEsting () = 
        interface ITesting
        with member x.Hey = 123

    type MarkdownElement =
        { Title: string; }
//        interface IHaveTitle with 
//            member x.Title = x.Title
//        interface ITestVal with
//            member x.Zzz = 654
    
    module MarkdownDom =
        let create name =
            { Title = name } 


module internal ContentMatching =

    let isMatch regex input = Regex(regex, RegexOptions.IgnoreCase).IsMatch(input)
    let checkRegex regex input = if isMatch regex input then Some input else None

    let (|IsMatch|_|) regex input = checkRegex regex input
    let (|IsDate|_|)   input = input |> checkRegex "(\d{1,4})-(\d{1,2})-(\d{1,2})"
    let (|IsBool|_|)   input = input |> checkRegex "(true|false)"
    let (|IsInt|_|)    input = input |> checkRegex "(\d{1,9})"
    let (|IsDouble|_|) input = input |> checkRegex "(\d{1,15})(,|\.)(\d{1,8})"


module Provide =

    open ContentMatching 
    open Provider

    let proxyType typeName =
        ProvidedTypeDefinition(Provider.assembly, Provider.namespace', 
                               typeName, Some typeof<MarkdownSource>)

    let markdownProxy filename generatedTypeName = 
        let proxyType = proxyType generatedTypeName
        proxyType.AddMember(ProvidedConstructor([], InvokeCode =  fun [] -> <@@ new MarkdownSource(filename) @@>))
        proxyType.AddMember(ProvidedConstructor(
                                [ProvidedParameter("filename", typeof<string>)],
                                InvokeCode = fun (Singleton filename) -> <@@ new MarkdownSource(%%filename) @@>))
        proxyType

    let date str = DateTime.Parse(str)
    let bool str = bool.Parse(str)
    let float str = 
        let sep = CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
        Double.Parse((str:string).Replace(".", sep).Replace(",", sep))
    
   // let mutable counter = 1
    
    let propFor (containerType : ProvidedTypeDefinition) token =
        let (type', getter:Quotations.Expr) =
            match token with
            | Root(i,c) | Header(i,c) ->
                let title = token.Title
                //counter <- counter + 1
                //let i = ProvidedTypeDefinition("I" + (String.replicate counter "a"), None, IsErased = false)
                //i.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
                //containerType.AddMember i


                //containerType.AddMember typeof<IHaveTitle>   // /zzz/ Not working... Adding interface to the record works, natch. Try and replicate the success by providing an implementation.  EMpty interfaces might be filtered out...



                (containerType :> Type), <@@ MarkdownDom.create title @@>
            | Property _ | Yaml _ -> 
                match token.Content with
                | IsBool c   -> typeof<bool>,     <@@ bool c   @@> 
                | IsDate c   -> typeof<DateTime>, <@@ date c   @@>
                | IsDouble c -> typeof<float>,    <@@ float c  @@> 
                | IsInt c    -> typeof<int>,      <@@ int c    @@> 
                | c          -> typeof<string>,   <@@ string c @@> 

        ProvidedProperty(propertyName = token.Title, 
                         propertyType = type', 
                         GetterCode = fun args -> getter)

    let interfaces (tree:TokenTree) = (tree, tree |> Interface.tree)
    
    open System
    open System.Reflection
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Quotations

    [<InterfaceAttribute>]
    type INum =
        abstract GetValue : unit -> int

    [<InterfaceAttribute>]
    type IHaveBaz =
        abstract Baz : string

        //render the ITree into actual types and then attach them to the container type
        //containerTy.AddInterfaceImplementation typeof<generated interface...>

        // Start out attaching simple props to objects

        // Need to grab the current interface (and feed them to the recursive algo underneath)
        // Need to test for interface `fitness`
        // Need to generate the type
        // Need to attach the type

    let rec properties parentTy ((Node(token, subtree):TokenTree), interfaces:ITree) =
        let containerTy = ProvidedTypeDefinition(token.Title + "Container", Some typeof<MarkdownElement>)
        let prop = token |> propFor containerTy 
        
        for node in subtree do 
            (node, interfaces) |> properties containerTy 

            // attach a generated interface for a known item to check functionality...


//        if token.Title = "Bar" then
//
//            let nameProp = ProvidedProperty(propertyName = "RawRawr", 
//                                        propertyType = typeof<string>,
//                                        GetterCode = fun _ -> <@@ "Aroooooo" @@>)
//            containerTy.AddMember nameProp
//
//            let createLocalInterface =
//
//
//                let i = ProvidedTypeDefinition("IAmAnInterfaaaaace", None, IsErased = false)
//                i.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
//                printf "I AM ATTACHING a property....\r\n"
//                let nameProp = ProvidedProperty(propertyName = "RawRawr", 
//                                                    propertyType = typeof<string>,
//                                                    GetterCode = fun _ -> <@@ () @@>)
//
//                i.AddMember nameProp
//                i
//            let local = createLocalInterface
//            containerTy.AddMember local
//            containerTy.AddInterfaceImplementation local

        parentTy.AddMember prop
        parentTy.AddMember containerTy
        // propOrDfault // attach a property or a dummy prop depending on the demands of the itnerface...    



    // TODO: the markdown source should provide a "toFile" method which uses the provided file names to call its properties

        

open Provider 

[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
    
    let createInterface =
        let i = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', "IZimbo", None, IsErased = false)
        i.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
        i

    let createLocalInterface =
        let i = ProvidedTypeDefinition("IZimbo", None)
        i.SetAttributes (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        let nameProp = ProvidedProperty(propertyName = "Name", 
                                         propertyType = typeof<string>,
                                         GetterCode = fun _ -> <@@ () @@>)

        i.AddMember nameProp
        i

    let testInterfaceImplementer = 
        let roo = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', "InterfaceImpl", Some typeof<MarkdownTesting>)
        roo.AddMember(ProvidedConstructor([], InvokeCode =  fun _ -> <@@ new MarkdownTesting() @@>))
        let local = createLocalInterface
        roo.AddMember local
        roo.AddInterfaceImplementation local
        roo
        
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
                |> Provide.interfaces
                |> Provide.properties proxyType 

                
            | Provider.MultiFile -> 

                let docCollectionType = ProvidedTypeDefinition("MarkdownSequence", Some typeof<obj>, HideObjectMethods = true)

                let docListType = ProvidedTypeDefinition("MarkdownFile", Some typeof<MarkdownFile>, HideObjectMethods = true)
                

                for file in source |> markdownFiles do
                    let docType = ProvidedTypeDefinition("DocumentContainer" + (file |> MarkdownFile.safeName), 
                                                         Some typeof<MarkdownFile>)
                    let docProp = ProvidedProperty(propertyName = (file |> MarkdownFile.safeName), 
                                                   propertyType = docType, 
                                                   GetterCode = fun args -> <@@ new MarkdownFile(file) @@>)
                    file
                    |> Parse.file  
                    |> Provide.interfaces
                    |> Provide.properties docType 

                    docCollectionType.AddMember docProp
                    proxyType.AddMember docType

                //proxyType.AddInterfaceImplementation

                proxyType.AddMember(ProvidedProperty(
                                        "Documents", docCollectionType,
                                        GetterCode = fun _ -> <@@ new obj() @@>))
                proxyType.AddMember docCollectionType

                proxyType.AddMember(ProvidedProperty(
                                        "Docs", typedefof<list<_>>.MakeGenericType(docListType),
                                        GetterCode = fun (Singleton source) -> <@@ (%%source: MarkdownSource).Files @@>))
                proxyType.AddMember docListType

                ()
            proxyType
        )
        proxyRoot
    
    do this.AddNamespace(Provider.namespace', [ createProxy; ]) //testInterfaceImplementer; 
    

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