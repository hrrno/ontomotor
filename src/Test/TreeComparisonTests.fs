
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



// we have:

    // Aggregate matches (ie all items have a property if any have a property (limit interface to one level?))    
    
    // Unioned matches (ie only the items that exist for all children will be captured)

// goals:

    // List out the common properties
    // List out the aggregate properties

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

type IItem = { Name : string; }

type MyUnion =
    | A of int * string
    | B of int * string
    with member x.Id   = match x with | A (i,n) | B (i,n) -> i
         member x.Name = match x with | A (i,n) -> n | _ -> "none"


type ITree = 
    | IFace of IItem * ITree list
    | IProp of IItem
    with member x.Item = match x with | IFace(n,sub) -> n | IProp (n) -> n
         member x.Sub  = match x with | IFace(n,sub) -> sub | IProp _ -> []
         member x.Props = x.Sub |> List.filter (function | IProp _ -> true | _ -> false)
         member x.Faces = x.Sub |> List.filter (function | IFace _ -> true | _ -> false)
                 
let props item = match item with | IProp _ -> true | _ -> false
let ifaces item = match item with | IFace _ -> true | _ -> false
let justProps item = (item:ITree).Sub |> List.filter props 
let justIFaces item = (item:ITree).Sub |> List.filter ifaces 

let rec findProps (Node(token, subTokens):TokenTree) : ITree =
    match token with 
        | Header (i,c) | Root (i,c) -> 
            let item = { Name  = "I" + token.Title; }
            let sub  = [ for i in subTokens do yield findProps i ]
            IFace(item, sub)     
        | Property (i,c) | Yaml (i,c) -> 
            IProp({ Name = token.Title })

let rec printProps (tree:ITree) =
    printfn "interface: %s = %s" tree.Item.Name tree.Item.Name
    match tree with
    | IProp _ -> ()
    | IFace (item, sub) -> 
        for s in sub do printProps s


let iTree = findProps t2


let printo li =  [for e in li do yield sprintf "%A" e ] |> String.concat ", "


let mutable accumulatorSeed : Set<ITree> ref = ref ([] |> Set.ofList)
let withSubtree s = !(s:Set<ITree> ref) |> Set.filter (fun item -> not (item.Sub.IsEmpty))
let removeFrom s item = (s:Set<ITree> ref) := (!s).Remove item
let listFromRef s = !s |> Set.toList

let propSet item = item |> justProps |> set
let faceSet item = item |> justIFaces |> set
let isPropertySubsetOf s2 s1 = Set.isSubset (s1 |> propSet) (s2 |> propSet)
let isInterfaceSubsetOf l2 l1 = Set.isSubset (set (l1:ITree).Sub) (set (l2:ITree).Sub)



//TODO: When merging property subsets sub interfaces are not merged accordingly
//      I believe a recursive merge function needs to be developed and applied to all sub properties
//      to get true sticky 'mergin' of the interfaces

// have to support recursive merge here to put trees together...
let previousMergeStrategy newInterface iface = 
    let faces = Set.union (newInterface |> faceSet) (iface |> faceSet)
    let props = Set.union (newInterface |> propSet) (iface |> propSet)
    let finalSub = Set.union (newInterface.Sub |> set) (iface.Sub |> set) |> Set.toList
    ()


let wrappedFace subs = IFace( { Name = "IShared" }, subs |> Seq.toList )
//
//let rec deepMerge (lhs : ITree) (rhs : ITree) : ITree =
//
//
//    printfn "\r\n::Deepmerge\r\n++++++++\r\n"
//    printfn "LHS::> %A\r\n" lhs
//    printfn "RHS::> %A\r\n" rhs
//    
//
//    let lhsFaces, lhsProps = lhs.Faces, lhs.Props
//    let rhsFaces, rhsProps = rhs.Faces, rhs.Props
//
//
//
//    
//    let finalFaces = lhsFaces @ rhsFaces |> set ... deepMerge rhsFaces lhsFaces ...
//    let finalProps = lhsProps @ rhsProps |> set
//
//
//
//    let combiFaces = lhsFaces @ rhsFaces
//    let final = 
//        combiFaces 
//        |> List.fold
//            (fun (combi :Set<ITree>) node ->
//                
//                printfn "Folding...\r\n"
//                printfn "Acc::> %A\r\n" combi
//                let newNew = deepMerge (combi |> Seq.head) node
//                printfn "Merg::> %A\r\n" newNew
//                combi.Add newNew 
//            )
//            (([ IFace( { Name = "IShared" }, [] ) ] : ITree list) |> set)
//
//
//    let combiProps = lhsProps @ rhsProps |> set
//    let fullProps = Set.union final combiProps
////    let combiProps = 
////        lhsProps @ rhsProps //@ final 
////        |> set 
////        |> Set.union final
////        |> Set.toList
//    printfn "\r\n. . . . . . . . . . . . . . . . .\r\n\
//             Finalacc::> %A\r\n\r\nFullret::>%A\r\n\r\n--------------------------\r\n\r\n" final (IFace( { Name = "IShared" }, fullProps |> Set.toList))
//    IFace( { Name = "IShared" }, fullProps |> Set.toList)
    // grab sub-sub interfaces and merge them
        // return the new merged items with a merged item?

    // let finalSub = Set.union (newInterface.Sub |> set) (iface.Sub |> set) |> Set.toList
    // newInterface <- IFace( { Name = "IShared" }, finalSub )






//    printfn "Meeeerging\r\n"
//    let lhsSubs = lhs |> justIFaces
//    let rhsSubs = rhs |> justIFaces
//    printfn "%A %A\r\n" lhsSubs rhsSubs
//
//    let lhsProps = lhs |> List.map justProps
//    let rhsProps = rhs |> List.map justProps
//    let props = lhsProps @ rhsProps |> set
//    
//    let combiFaces = deepMerge lhsSubs rhsSubs |> set
//
//    combiFaces 
//    |> Set.union props
//    |> wrappedFace

//
//    let faces = deepMerge lhsSubs rhsSubs
//    let faces = Set.union (newInterface |> faceSet) (iface |> faceSet)
//    let props = Set.union (newInterface |> propSet) (iface |> propSet)
//    let finalSub = Set.union (newInterface.Sub |> set) (iface.Sub |> set) |> Set.toList
    
let interfacesWithProperties = function | IFace (n, []) -> false | _ -> true
let mergedIFace subs = IFace ({ Name = "IAmMerged" }, subs)
let emptyIFace = mergedIFace []

let rec deepMerge (lhs : ITree) (rhs : ITree) : ITree = 
    let mergedFaces = 
        lhs.Faces @ rhs.Faces 
        |> List.fold deepMerge emptyIFace

    [mergedFaces] @ lhs.Props @ rhs.Props 
    |> List.filter interfacesWithProperties
    |> set 
    |> Set.toList
    |> mergedIFace


let rec interfaceTree (tree : ITree) =
    match tree.Sub with
    | [] -> [ IFace( { Name = "IAmEmpty" }, [] )]
    | x::xs -> 
        tree.Sub
        |> List.fold 
            (fun (interfaces:Set<ITree> ref) node -> 
                let mutable newInterface = 
                    match node with
                    | IFace _ -> IFace( { Name = "IShared" }, node |> interfaceTree )
                    | IProp _ -> node
                
                for iface in interfaces |> withSubtree do 
                    if iface |> isInterfaceSubsetOf newInterface then
                        iface |> removeFrom interfaces

                    else if iface |> isPropertySubsetOf newInterface || newInterface |> isPropertySubsetOf iface then 
                        
                        printfn "\r\n--------------------------\r\nDeep merging\r\n"

                        iface |> removeFrom interfaces
                        newInterface <- deepMerge newInterface iface

                        //let finalSub = Set.union (newInterface.Sub |> set) (iface.Sub |> set) |> Set.toList
                        //newInterface <- IFace( { Name = "IShared" }, finalSub )
                        
                ref ((!interfaces).Add newInterface)
                )
            accumulatorSeed 
        |> listFromRef



let t5t = t5 |> findProps |> interfaceTree



printfn "%s" (new System.String('\n', 3))
let t1t = t1 |> findProps |> interfaceTree
let t2t = t2 |> findProps |> interfaceTree
let t3t = t3 |> findProps |> interfaceTree
let t4t = t4 |> findProps |> interfaceTree




// Split the trees branches into recursive sets: top and rest, fold top into rest
//  Depending on merge logic either union or join
//     Name conflicts and different types *should* result in a downcast (right?)
//     Not liking the idea of modification... merge rules to change result?  Datatype "take the lowest of these"

// Identify commonalities - tag?
    // Create an aggregate
    // Create a union

    // Split the children into two trees and then return a result (union or join), of their children
        // headers/"interfaces" are ignored, 


// TODO: figure out how to handle conflicting data types ie IOne.Prop : int vs ITwo.Prop : string [(an exception?)]




module Interface =

    let extract (t1 : TokenTree) (t2: TokenTree) =
        
        t1.Print
        t2.Print

Interface.extract t1 t2



