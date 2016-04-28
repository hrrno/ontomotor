
module Ontomotor.TreeComparisonTests


open NUnit.Framework
open FsUnit
open System
open OntologyStructure 
open MarkdownParser
open MarkdownStructure

let inline add x y = x + y


//[<Test>]
//let ``When 2 is added to 2 expect 4``() = 
//    add 2 2 |> should equal 4 
//
//
//
//
//[<TestFixture>]
//type AccountTest() =
//  [<Test>]
//  member x.SimpleTestico() = 
//    1 |> should equal 1
//    
//[<Test>]
//let ``When trees have matching content``() = 
//    add 2 2 |> should equal 4 
//
//[<Test>]
//let ``When trees have no matching content``() = 
//    add 2 2 |> should equal 4 
//
//[<Test>]
//let ``When trees have partially matching content``() = 
//    add 2 2 |> should equal 4 



open MarkdownParser.Tokenize
open Lex

let t1 = [ Root(0, "Root")
           Header(10, "# FirstHeader")
           Property(20, "firstprop: wow")            
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
         ] |> tokenTree

let t2 = [ Root(0, "Root")
           Header(10, "# FirstHeader")
           Property(15, "firstprop: wow")            
           Property(20, "secondprop: wdow")                
           Header(25, "# FirstHeader clone")
           Property(28, "firstprop: wow")            
           Property(29, "secondprop: wdow")          
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Property(60, "thirdprop: bowtt") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(90, "subsubprop: bowtt") 
         ] |> tokenTree



let t3 = [ Root(0, "Root")
           Header(10, "# FirstHeader")
           Property(15, "firstprop: wow")            
           Property(20, "secondprop: wdow")    
           Property(60, "thirdprop: bowtt")                        
           Header(25, "# FirstHeader clone")
           Property(28, "firstprop: wow")            
           Property(29, "secondprop: wdow")          
           Property(29, "thirdprop: wdow")                        
           Header(25, "# 2 FirstHeader clone 2")
           Property(28, "firstprop: wow")            
           Property(29, "secondprop: wdow")          
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(90, "subsubprop: bowtt") 
         ] |> tokenTree



let t4 = [ Root(0, "Root")
           Header(10, "# FirstHeader")
           Property(15, "firstprop: wow")            
           Property(20, "secondprop: wdow")                         
           Header(25, "# FirstHeader clone")
           Property(28, "firstprop: wow")            
           Property(29, "secondprop: wdow")          
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(90, "subsubprop: bowtt") 
           Header(30, "# SecondHeader 2")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(80, "nomatchsubprop: bowtt") 
           Property(90, "nomatchsubsubprop: bowtt") 
         ] |> tokenTree


// loosely representing two trees with non-matched sub interfaces
let t5 = [ Root(0, "Root")
           Header(10, "# LHS FirstHeader")
           Property(20, "firstprop: wow")  
           Property(20, "secondprop: wow")  
           Header(30, "## LHS secondheader")
           Property(40, "lhsfirstprop: wow")      
           Property(40, "lhssecondprop: bow") 
           Header(50, "# RHS FirstHeader")
           Property(60, "firstprop: wow")  
           Property(60, "secondprop: wow")  
           Header(70, "## RHS secondheader")
           Property(80, "firstprop: wow")      
           Property(80, "secondprop: bow") 
           Header(70, "### RHS thirdheader")
           Property(80, "firstprop: wow")      
           Property(80, "secondprop: bow") 
           Header(70, "#### RHS fourthheader")
           Property(80, "firstprop: wow")      
           Property(80, "secondprop: bow") 
         ] |> tokenTree


open MarkdownStructure.Interface
//open ProviderImplementation.ProvidedTypes
open System.Collections.Generic



type ProvidedProperty(propertyName: string, propertyType: Type) = 
    member c.foo = "rawrawr"


type ProvidedTypeDefinitionOOO(className : string, baseType: Type option) =
    member x.AddMember (m:System.Reflection.MethodInfo) = ()
    member x.AddMember (m:ProvidedProperty) = ()
    member x.AddMember (m:System.Reflection.PropertyInfo) = ()
    member x.AddMember (m:ProvidedTypeDefinitionOOO) = ()
    member x.ToType = typeof<string>

type MarkdownElement =
        { Title: string; }

let makeProp (foo:ITree) = 
    let prop = match foo with | IProp el -> el | _ -> failwith "Properties should only come from IProp's"
    ProvidedProperty(propertyName = prop.Name,  propertyType = typeof<string>)


//let rec baseclassTypeTree (tree:ITree) =
//    let faces, props = tree.Faces, tree.Props
//    let newType = ProvidedTypeDefinition(tree.Item.Name + "Base", Some typeof<MarkdownElement>)
//    for f in faces do 
//            newType.AddMember (f |> baseclassTypeTree)
//
//    for p in props do
//        newType.AddMember (p |> makeProp)
//    newType


module Interface =


    let rec tree (Node(token, subTokens):TokenTree) : ITree =
        match token with 
            | Header (i,c) | Root (i,c) -> 
                let item = { Name  = "I" + token.Title; }
                let sub  = [ for s in subTokens do yield tree s ]
                IFace(item, sub)
            | Property (i,c) | Yaml (i,c) -> 
                IProp({ Name = token.Title })

    let rec isContainedBy (merged:ITree) (face:ITree) =   
        let propsAreContained = 
            face.Props
            |> List.fold (fun acc prop -> acc && merged.Props |> List.exists ((=) prop)) true

        let facesAreContained =
            face.Faces
            |> List.fold (fun acc face -> acc && merged.Faces |> List.exists (fun mface -> face |> isContainedBy mface) ) true       
            
        propsAreContained && facesAreContained

    let mergedParentInterface (face:ITree) (merged:ITree) : Dictionary<IItem,ITree> =
        let map = new Dictionary<IItem,ITree>()
        let rec mapTrees (face:ITree) (merged:ITree) =
            for i in face.Faces do
                for m in merged.Faces do
                    if i |> isContainedBy m then
                        map.Add (i.Item, m)
                        mapTrees i m
                    else
                        printf "No match found for '%s'\r\n" i.Item.Name
        mapTrees face merged 
        map



    let itemMap (interfaceMap:Dictionary<IItem,ITree>) (typeMap:Dictionary<ITree,ProvidedTypeDefinitionOOO>) : Dictionary<IItem, ProvidedTypeDefinitionOOO> = 
        let map = new Dictionary<IItem, ProvidedTypeDefinitionOOO>()
        for KeyValue(k,v) in interfaceMap do
            map.Add (k, typeMap.Item (v))
        map


    let rec baseclassTypeTree (tree:ITree) =
        let faces, props = tree.Faces, tree.Props
        let newType = ProvidedTypeDefinitionOOO(tree.Item.Name + "Base", Some typeof<MarkdownElement>)
        for f in faces do 
                newType.AddMember ((f |> baseclassTypeTree):ProvidedTypeDefinitionOOO)

        for p in props do
            newType.AddMember (p |> makeProp)
        newType
        //(tree, newType)


    let typeMap (tree:ITree) : Dictionary<ITree,ProvidedTypeDefinitionOOO> =
        let map = new Dictionary<ITree, ProvidedTypeDefinitionOOO>()
        let mutable counter = 0
        let rec generate (tree:ITree) =
            counter <- counter + 1
            let newType = ProvidedTypeDefinitionOOO(tree.Item.Name + "Base" + counter.ToString(), Some typeof<MarkdownElement>)
            for f in tree.Faces do 
                    newType.AddMember ((f |> generate):ProvidedTypeDefinitionOOO)
            for p in tree.Props do
                newType.AddMember (p |> makeProp)
            map.Add (tree, newType)
            newType
        tree |> generate |> ignore
        map

    type TokenInterfaceTree = | InterfaceTree of Token * IItem * TokenInterfaceTree list

    let rec decoratedTree (Node(token, subTokens):TokenTree) : TokenInterfaceTree =
        match token with 
            | Header (i,c) | Root (i,c) -> 
                let item = { Name  = "I" + token.Title; }
                let sub  = [ for s in subTokens do yield decoratedTree s ]
                InterfaceTree(token, item, sub)
            | Property (i,c) | Yaml (i,c) -> 
                InterfaceTree(token, { Name = token.Title }, [])


    let registerBaseTypes (types:IEnumerable<Type>) =

        ////////////////////////


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
        let baseTypeMap = typeMap merged
        let itemMap = itemMap interfaceMap baseTypeMap   /// this isn`t going to work - the maps are indexed by the shared name in the typeMap

        //registerBaseTypes baseTypeMap.Values

        tokens, itemMap
        
        
    let propFor (containerType : ProvidedTypeDefinitionOOO) (token : Token) =
//        let (type', getter:Quotations.Expr) =
//            match token with
//            | Root(i,c) | Header(i,c) ->
//                let title = token.Title
//                (containerType :> Type), <@@ MarkdownDom.create title @@>
//            | Property _ | Yaml _ -> 
//                match token.Content with
//                | IsBool c   -> typeof<bool>,     <@@ bool c   @@> 
//                | IsDate c   -> typeof<DateTime>, <@@ date c   @@>
//                | IsDouble c -> typeof<float>,    <@@ float c  @@> 
//                | IsInt c    -> typeof<int>,      <@@ int c    @@> 
//                | c          -> typeof<string>,   <@@ string c @@> 

        ProvidedProperty(propertyName = token.Title, 
                         propertyType = typeof<string>)

    let rec properties parentTy ((Node(token, subtree):TokenTree), typeMap:Dictionary<IItem,Type>) =
        //let baseType = typeMap.Item(token)
        let containerTy = ProvidedTypeDefinitionOOO(token.Title + "Container", Some typeof<MarkdownElement>)
        let prop = propFor containerTy token
        
        for node in subtree do 
            (node, typeMap) |> properties containerTy 
        parentTy.AddMember prop
        parentTy.AddMember containerTy

let t1t = t1 |> Interface.mergedTree
let t2t = t2 |> Interface.mergedTree
let t3t = t3 |> Interface.mergedTree
let t4t = t4 |> Interface.mergedTree
let t5t = t5 |> Interface.mergedTree

let t1d = t1 |> decoratedTree
let t2d = t2 |> decoratedTree
let t3d = t3 |> decoratedTree
let t4d = t4 |> decoratedTree
let t5d = t5 |> decoratedTree

let t5i = t5 |> Interface.tree



t5i |> Interface.print
t5t |> Interface.print
let rawr = Interface.mergedParentInterface t5i t5t

//t1t |> Interface.typeMap


let rec decoratedTokens (Node(token, subTokens):TokenTree) : Token * IItem =
    match token with 
        | Header (i,c) | Root (i,c) -> 
            let item = { Name  = "I" + token.Title; }
            //let sub  = [ for s in subTokens do yield (decoratedTree s |> snd) ]
            token, item     
        | Property (i,c) | Yaml (i,c) -> 
            token, { Name = token.Title }