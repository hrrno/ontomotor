
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
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic



type MarkdownElement =
        { Title: string; }

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



type TokenInterfaceTree = | InterfaceTree of Token * IItem * TokenInterfaceTree list

module Interface =

    let rec private printNode depth (t:ITree) =
        printfn "%s%s" depth (t.Item.Name.Replace("\r\n", "\r\n" + depth))
        for s in t.Sub do printNode (depth + "----") s

    let print (t:ITree) = printNode "" t

    let rec decoratedTree (Node(token, subTokens):TokenTree) : TokenInterfaceTree =
        match token with 
            | Header (i,c) | Root (i,c) -> 
                let item = { Name  = "I" + token.Title; }
                let sub  = [ for s in subTokens do yield decoratedTree s ]
                InterfaceTree(token, item, sub)
            | Property (i,c) | Yaml (i,c) -> 
                InterfaceTree(token, { Name = token.Title }, [])

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
            |> List.fold (fun acc prop -> acc && merged.Props |> List.contains prop) true

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


let typeMap (tree:ITree) : Dictionary<ITree,Type> =
    let map = new Dictionary<ITree, Type>()
    let rec generate (tree:ITree) =
        let newType = ProvidedTypeDefinition(tree.Item.Name + "Base", Some typeof<MarkdownElement>)
//        for f in tree.Faces do 
//                newType.AddMember (f |> generate)
//        for p in tree.Props do
//            newType.AddMember (p |> makeProp)
        map.Add (tree, newType)
        newType
    tree |> generate |> ignore
    map
    
    
let interfaces (tree:TokenTree) = 

    // takes a tree of tokens
    // pulls out a merged interface
    // maps the tokens to an iface tree
    // maps the iface tree to the merged tree
    // maps the tokens to the merged tree

    // replace the merged tree with real types
        
    // register base types
        
    // returns the merged type tree and the type map
    // return a TokenTree * Type tree

    let decorated = tree |> Interface.decoratedTree
    let mergedInterfaces = tree |> Interface.mergedTree
    let interfaces = tree |> Interface.tree
    let interfaceMap = Interface.mergedParentInterface interfaces mergedInterfaces
    let typeMap = typeMap mergedInterfaces
        
    let typeTree = mergedInterfaces |> baseclassTypeTree

    (tree, tree |> Interface.mergedTree)

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

t4t |> typeMap
