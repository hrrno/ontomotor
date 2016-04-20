
module Ontomotor.TreeComparisonTests


open NUnit.Framework
open FsUnit
open System
open OntologyStructure 
open MarkdownParser

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
    
[<Test>]
let ``When trees have matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have no matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have partially matching content``() = 
    add 2 2 |> should equal 4 



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





type MyUnion =
    | A of int * string
    | B of int * string
    with member x.Id   = match x with | A (i,n) | B (i,n) -> i
         member x.Name = match x with | A (i,n) -> n | _ -> "none"




module Interface =         

    type IItem = { Name : string; }

    type ITree = 
        | IFace of IItem * ITree list
        | IProp of IItem
        with member x.Item = match x with | IFace(n,sub) -> n | IProp (n) -> n
             member x.Sub  = match x with | IFace(n,sub) -> sub | IProp _ -> []
             member x.Props = x.Sub |> List.filter (function | IProp _ -> true | _ -> false)
             member x.Faces = x.Sub |> List.filter (function | IFace _ -> true | _ -> false)

    let rec print (tree:ITree) =
        printfn "interface: %s = %s" tree.Item.Name tree.Item.Name
        match tree with
        | IProp _ -> ()
        | IFace (item, sub) -> for s in sub do print s

    [<AutoOpen>]
    module private Merging = 

        let rec propertyTree (Node(token, subTokens):TokenTree) : ITree =
            match token with 
                | Header (i,c) | Root (i,c) -> 
                    let item = { Name  = "I" + token.Title; }
                    let sub  = [ for i in subTokens do yield propertyTree i ]
                    IFace(item, sub)     
                | Property (i,c) | Yaml (i,c) -> 
                    IProp({ Name = token.Title })

        let props item = match item with | IProp _ -> true | _ -> false
        let justProps item = (item:ITree).Sub |> List.filter props 
        let propSet item = item |> justProps |> set
        let isPropertySubsetOf s2 s1 = Set.isSubset (s1 |> propSet) (s2 |> propSet)
        let isPropertyMatchedWith s2 s1 = s1 |> isPropertySubsetOf s2 || s2 |> isPropertySubsetOf s1
        let isInterfaceSubsetOf l2 l1 = Set.isSubset (set (l1:ITree).Sub) (set (l2:ITree).Sub)
        let interfacesWithProperties = function | IFace (n, []) -> false | _ -> true
        let face (name, subs) = IFace ({ Name = name }, subs)
        let mergedFace subs = ("IShared", subs) |> face
        let emptyFace = ("IAmEmpty", []) |> face

        let (|InterfaceSubSet|_|)   (lhs, rhs) = if lhs |> isInterfaceSubsetOf   rhs then Some (lhs, rhs) else None
        let (|InterfaceSuperSet|_|) (lhs, rhs) = if rhs |> isInterfaceSubsetOf   lhs then Some (lhs, rhs) else None
        let (|PropertyMatch|_|)     (lhs, rhs) = if lhs |> isPropertyMatchedWith rhs then Some (lhs, rhs) else None

        let mutable accumulatorSeed : Set<ITree> ref = ref ([] |> Set.ofList)
        let withSubtrees s = !(s:Set<ITree> ref) |> Set.filter (fun item -> not (item.Sub.IsEmpty))
        let removeFrom s item = (s:Set<ITree> ref) := (!s).Remove item
        let addTo s item = (!(s:Set<ITree> ref)).Add item
        let toList s = !s |> Set.toList

        let rec deepMerge (lhs : ITree) (rhs : ITree) : ITree = 
            let mergedSubFace = lhs.Faces @ rhs.Faces |> List.fold deepMerge emptyFace

            [mergedSubFace] @ lhs.Props @ rhs.Props 
            |> List.filter interfacesWithProperties
            |> set 
            |> Set.toList
            |> mergedFace

        let merge (interfaces : Set<ITree> ref) (rhs : ITree) (lhs : ITree)  = 
            match (lhs, rhs) with
            | InterfaceSuperSet _ -> 
                lhs
            | InterfaceSubSet _ -> 
                lhs |> removeFrom interfaces
                rhs
            | PropertyMatch _ ->
                lhs |> removeFrom interfaces
                deepMerge rhs lhs
            | _ -> rhs

        let rec mergedTree (tree : ITree) =

            let extractInterface node = 
                match node with 
                | IFace _ -> ("IShared", node |> mergedTree) |> face
                | IProp _ -> node 

            let mergedWith interfaces node =
                interfaces 
                |> withSubtrees
                |> Set.fold (merge interfaces) (node |> extractInterface)

            let mergeAll (interfaces : Set<ITree> ref) node =
                node 
                |> mergedWith interfaces
                |> addTo interfaces
                |> ref

            match tree.Sub with
            | [] -> [ emptyFace ]
            | _ -> 
                   tree.Sub
                   |> List.fold mergeAll accumulatorSeed  
                   |> toList


       // test this....  tree based gen workin??

        let rec mergedTree2 (tree : TokenTree) =

            let extractInterface (node:TokenTree) = 
                match node.Token with 
                | Header _   | Root _ -> ("IShared", node |> mergedTree2) |> face
                | Property _ | Yaml _ -> IProp({ Name = node.Token.Title }) 

            let mergedWith interfaces node =
                interfaces 
                |> withSubtrees
                |> Set.fold (merge interfaces) (node |> extractInterface)

            let mergeAll (interfaces : Set<ITree> ref) node =
                node 
                |> mergedWith interfaces
                |> addTo interfaces
                |> ref

            match tree.Sub with  // replace with deconstruction in parameter??
            | [] -> [ emptyFace ]
            | _ -> 
                   tree.Sub
                   |> List.fold mergeAll accumulatorSeed  
                   |> toList

    let tree (tokens:TokenTree) = tokens |> (propertyTree >> mergedTree)
    let tree2 (tokens:TokenTree) = tokens |>  mergedTree2



printfn "%s" (new System.String('\n', 3))
let t1t = t1 |> Interface.tree
let t2t = t2 |> Interface.tree
let t3t = t3 |> Interface.tree
let t4t = t4 |> Interface.tree
let t5t = t5 |> Interface.tree2



// TODO: figure out how to handle conflicting data types ie IOne.Prop : int vs ITwo.Prop : string [(an exception?)]




module Interface2 =

    let extract (t1 : TokenTree) (t2: TokenTree) =
        
        t1.Print
        t2.Print

Interface.extract t1 t2

