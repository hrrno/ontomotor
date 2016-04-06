
module Ontomotor.TreeComparisonTests


open NUnit.Framework
open FsUnit
open System
open OntologyStructure 
open MarkdownParser

let inline add x y = x + y

[<Test>]
let ``When 2 is added to 2 expect 4``() = 
    add 2 2 |> should equal 4 




[<TestFixture>]
type AccountTest() =
  [<Test>]
  member x.SimpleTestico() = 
    1 |> should equal 1




// we have:

    // Aggregate matches (ie all items have a property if any have a property (limit interface to one level?))    
    
    // Unioned matches (ie only the items that exist for all children will be captured)

    
[<Test>]
let ``When trees have matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have no matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have partially matching content``() = 
    add 2 2 |> should equal 4 

