namespace Proto.TypeProviderType

open System
open System.IO
open System.Globalization
open System.Text.RegularExpressions
open System.Reflection
open System.Collections.Generic
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

    type MarkdownElement =
        { Title: string; }

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
    
    
    let propFor (containerType : ProvidedTypeDefinition) token =
        let (type', getter:Quotations.Expr) =
            match token with
            | Root(i,c) | Header(i,c) ->
                let title = token.Title
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

    let makeProp (foo:ITree) = 
        let prop = match foo with | IProp el -> el | _ -> failwith "Properties should only come from IProp's"
        ProvidedProperty(propertyName = prop.Name, 
                         propertyType = typeof<string>, 
                         GetterCode = fun args -> <@@ "[emptyprop]" @@>)




    let rec baseclassTypeTree (tree:ITree) =
        let faces, props = tree.Faces, tree.Props
        let newType = ProvidedTypeDefinition(tree.Item.Name + "Base", Some typeof<MarkdownElement>)
        for f in faces do 
                newType.AddMember (f |> baseclassTypeTree)

        for p in props do
            newType.AddMember (p |> makeProp)
        newType
        //(tree, newType)


    let typeMap (tree:ITree) : Dictionary<ITree,Type> =
        let map = new Dictionary<ITree, Type>()
        let mutable counter = 0
        let rec generate (tree:ITree) =
            counter <- counter + 1
            let newType = ProvidedTypeDefinition(tree.Item.Name + "Base" + counter.ToString(), Some typeof<MarkdownElement>)
            for f in tree.Faces do 
                    newType.AddMember (f |> generate)
            for p in tree.Props do
                newType.AddMember (p |> makeProp)
            map.Add (tree, newType)
            newType
        tree |> generate |> ignore
        map



    let registerBaseTypes (types:IEnumerable<Type>) =

        ////////////////////////

           Think I need to bring back the Token * IItem * Subs data structure, as its use case is in the property mapping

//        1) make provided containers inherit from their mapped types.. Some typeof<MarkdownElement> => typeMap.Item(token.Item)
//        2) register the types (right here in this function)
//        3) test
//        4) refactor

        // this.Namespace.AddMembers types
        ////////////////////////
        ()

    let interfaces (tokens:TokenTree) = 
    
        let interfaces = tokens |> tree
        let merged = tokens |> mergedTree
        let interfaceMap = mergedParentInterface interfaces merged
        let typeMap = typeMap merged
        let itemMap = itemMap interfaceMap typeMap

        registerBaseTypes typeMap.Values

        tokens, itemMap
        
        

    let rec properties parentTy ((Node(token, subtree):TokenTree), typeMap:Dictionary<IItem,Type>) =
        let baseType = typeMap.Item(token)
        let containerTy = ProvidedTypeDefinition(token.Title + "Container", Some typeof<MarkdownElement>)
        let prop = token |> propFor containerTy 
        
        for node in subtree do 
            (node, typeMap) |> properties containerTy 
        parentTy.AddMember prop
        parentTy.AddMember containerTy
        

    // TODO: the markdown source should provide a "toFile" method which uses the provided file names to call its properties

        

open Provider 

[<TypeProvider>]
type ProtoTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()
    

    let inheritenceTest =
        let zBase =  ProvidedTypeDefinition(Provider.assembly, Provider.namespace', "BaseClass", Some typeof<MarkdownTesting>)
        zBase.AddMember(ProvidedConstructor([], InvokeCode =  fun _ -> <@@ new MarkdownTesting() @@>))
        let nameProp = ProvidedProperty(propertyName = "Nameo", propertyType = typeof<string>,GetterCode = fun _ -> <@@ "And bingo was his..." @@>)
        zBase.AddMember nameProp
        
        let zChild = ProvidedTypeDefinition(Provider.assembly, Provider.namespace', "ChildClass", Some (zBase :> Type))
        zChild.AddMember(ProvidedConstructor([], InvokeCode =  fun _ -> <@@ new MarkdownTesting() @@>))

        let extraProp = ProvidedProperty(propertyName = "Extra", propertyType = typeof<string>,GetterCode = fun _ -> <@@ "This is another value" @@>)
        zChild.AddMember extraProp

        let overrideProp = ProvidedProperty(propertyName = "Nameo", propertyType = typeof<string>, GetterCode = fun _ -> <@@ "OVERIDDEN" @@>)
        zChild.AddMember overrideProp
        
        zBase, zChild

        
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
                    let docType = ProvidedTypeDefinition("DocumentContainer" + (file |> MarkdownFile.safeName), Some typeof<MarkdownFile>)
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

                proxyType.AddMember(ProvidedProperty("Documents", docCollectionType, GetterCode = fun _ -> <@@ new obj() @@>))
                proxyType.AddMember docCollectionType

                proxyType.AddMember(ProvidedProperty("Docs", typedefof<list<_>>.MakeGenericType(docListType),
                                    GetterCode = fun (Singleton source) -> <@@ (%%source: MarkdownSource).Files @@>))
                proxyType.AddMember docListType

                ()
            proxyType
        )
        proxyRoot
    
    let lhs, rhs = inheritenceTest
    do this.AddNamespace(Provider.namespace', [ createProxy; lhs; rhs; ])
    

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