
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
           Header(30, "# SecondHeader")
           Property(40, "firstprop: bow") 
         ] |> tokenTree



type IItem = { Name : string; }
type IProp = { PName : string; }

type ITree = 
    | Props of IItem * ITree list
    with member x.IFace = match x with | Props(n,sub) -> n
         member x.Sub   = match x with | Props(n,sub) -> sub

//        
//let rec buildTree trees (list : ITree list) = 
//    match list with
//    | [] -> trees, [] 
//    | (token)::xs ->
//        let rec collectSubTrees xs trees = 
//            match buildTree [] xs with
//            | [], rest -> trees, rest
//            | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
//        let sub, rest = collectSubTrees xs []
//        [Props(token, sub)], rest
//
//
//
//let foo (t:TokenTree) = buildTree [] t.Sub

let rec findProps (t:TokenTree) : ITree =
    match t.Token with
        | Property (i, c) -> Props({ Name = t.Token.Title }, [])
        | Header (i,c) | Header (i,c) -> 
            let item = { Name  = "I" + t.Token.Title; }
            let sub = match t.Sub with
                            | [] -> []
                            | x::xs -> [ for i in t.Sub do yield findProps i ]
            Props(item, sub)
        
        | _ -> failwith "Not handling all cases yet"
    
//    let foo = [
//                for s in t.Sub do
//                    let n = "I" + s.Token.Title
//                    yield
//                        match s.Sub with
//                        | [] -> Props({ Name = n; }, []) 
////                        | [x] -> 
////                            x.Print
////                            Props({ Name = "oi"; }, findProps x) 
//                        | x::xs -> 
//                            x.Print
//                            Props({ Name = "boo"; }, [ for t in s.Sub do yield! findProps t ]) 
//                        //| [a,b]
//                        //| [_] -> Props({ Name = n; }, [ for t in s.Sub do yield! findProps t ])
//
////                    let s = 
////                        match s.Token with
////                        | Header (i,c) -> { Name  = n; }
////                        | Property (i, c) -> { PName = s.Token.Title }
////                    let ps = s.Sub |> List.map findProps
////                    yield  Props({ Name = n; }, [ sub ]) 
//              ]
//    foo        

findProps t1

module Interface =

    let extract (t1 : TokenTree) (t2: TokenTree) =
        
        t1.Print
        t2.Print

Interface.extract t1 t2



