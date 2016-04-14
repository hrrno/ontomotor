
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
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(90, "subsubprop: bowtt") 
         ] |> tokenTree


type IItem = { Name : string; }

type ITree = 
    | IFace of IItem * ITree list
    | IProp of IItem
    with member x.Item = match x with | IFace(n,sub) -> n | IProp (n) -> n
         member x.Sub  = match x with | IFace(n,sub) -> sub | IProp _ -> []
//         member x.Id = 
//            match x with 
//            | IFace (item,sub) -> 
//                sub |> List.fold (fun acc subItem -> acc + subItem.Id) 0
//            | IProp (i) -> i.Name.GetHashCode()

type IDecoratedTree =
    | IDFace of IItem * interfaces : string list * IDecoratedTree list
    | IDProp of IItem
        

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
//iTree |> printProps


// there are two cases to handle: 1 looking for children, 2 comparing trees against one another
        // Should the comparison be done one the content or the interfaceextraction?


// clarify the use case a little, use a clear example showing both situations 
    // a list with repeating children
    // two lists with overlap

                // send in a flag as a param so that the top level read is intersecting 
                // while lower level reads are "adding"?



  // extract interface from x
        // grab interfaces from XS
        // compare and merge

// pull out subsets
    // remove them
// check for supersets
    // if not, add the new one




let printo li =  [for e in li do yield sprintf "%A" e ] |> String.concat ", "
let mutable accumulatorSeed : Set<ITree> ref = ref ([] |> Set.ofList)
let withChildren s = (s:Set<ITree>) |> Set.filter (fun item -> not (item.Sub.IsEmpty))
let removeFrom (s:Set<ITree> ref) item = s := (!s).Remove item
let listFromRef s = !s |> Set.toList
let props item = match item with | IProp _ -> true | _ -> false
let ifaces item = match item with | IFace _ -> true | _ -> false
let justProps item = (item:ITree).Sub |> List.filter props 
let justIFaces item = (item:ITree).Sub |> List.filter ifaces 
let propSet l = l |> justProps |> set
let faceSet l = l |> justIFaces |> set
let isPropertySubsetOf s2 s1 = Set.isSubset (s1 |> justProps |> set) (s2 |> justProps |> set)
let isInterfaceSubsetOf l2 l1 = Set.isSubset (set (l1:ITree).Sub) (set (l2:ITree).Sub)




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

                for iface in !interfaces |> withChildren do 
                    if iface |> isInterfaceSubsetOf newInterface then
                        iface |> removeFrom interfaces

                    if iface |> isPropertySubsetOf newInterface 
                       || newInterface |> isPropertySubsetOf iface then 
                        let faces = Set.union (newInterface |> faceSet) (iface |> faceSet)
                        let props = Set.union (newInterface |> propSet) (iface |> propSet)
                        iface |> removeFrom interfaces
                        let finalSub =  Set.union faces props |> Set.toList
                        newInterface <- IFace( { Name = "IShared" }, finalSub )

                ref ((!interfaces).Add newInterface)
                )
            accumulatorSeed 
        |> listFromRef


printfn "%s" (new System.String('\n', 3))
//let t22 = t2 |> findProps
let t2t = t2 |> findProps |> interfaceTree

//let t32 = t3 |> findProps
let t3t = t3 |> findProps |> interfaceTree


type tree<'a> =
    | EmptyTree
    | TreeNode of 'a * 'a tree * 'a tree

let foo = EmptyTree

let data = [("Cats",4);
            ("Dogs",5);
            ("Mice",3);
            ("Elephants",2)]
let count = List.fold (fun acc (nm,x) -> acc+x) 0 data


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



