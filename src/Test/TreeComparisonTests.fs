module TreeComparisonTests


open NUnit.Framework
open FsUnit
open System


let inline add x y = x + y

[<Test>]
let ``When 2 is added to 2 expect 4``() = 
    add 2 2 |> should equal 4 



