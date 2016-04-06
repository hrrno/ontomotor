
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
           Property(20, "firstprop: wow")            
           Property(25, "secondprop: wdow")            
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
           Property(50, "secondprop: bodw") 
           Property(60, "thirdprop: bowtt") 
           Header(70, "## Subheader") 
           Property(80, "subprop: bowtt") 
           Property(90, "subsubprop: bowtt") 
         ] |> tokenTree



type IItem = { Name : string; }
//type IItem = { Name : string; }

type ITree = 
    | IFace of IItem * ITree list
    | IProp of IItem //* ITree list
    with member x.Item = match x with | IFace(n,sub) -> n | IProp (n) -> n
         member x.Sub  = match x with | IFace(n,sub) -> sub | IProp _ -> []

        //member x.Tag = match

        
        // tag property names and create a hash or a key(?) ignoring interface titles, but showing child interfaces


let rec findProps (Node(token, subTokens):TokenTree) : ITree =
    match token with 
        | Header (i,c) | Root (i,c) -> 
            let item = { Name  = "I" + token.Title; }
            let sub  = [ for i in subTokens do yield findProps i ]
            IFace(item, sub)     
        | Property (i,c) | Yaml (i,c) -> 
            IProp({ Name = token.Title } ) //, [])       

let iTree = findProps t2


// Identify commonalities - tag?
    // Create an aggregate
    // Create a union






module Interface =

    let extract (t1 : TokenTree) (t2: TokenTree) =
        
        t1.Print
        t2.Print

Interface.extract t1 t2



