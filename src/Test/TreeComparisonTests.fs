
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

type A() = 
  interface IDisposable with 
    member x.Dispose() = printfn "gone"

// Disposes the first element from an array
let disposeFirst (a:#IDisposable[]) = a.[0].Dispose()

// Call 'disposeFirst' with 'A[]' as an argument
let aa = [| new A(); new A() |]
disposeFirst aa


// downcasting
// generic constraint on the interface to provide casting? (force it to provide a cast function and use it)
type IAnother = interface end
[<AbstractClass>]
type IRaw () = 
    member x.Raw : obj = unbox x

type Caster<'a> = 
    member x.Cast  = x 

type WithExpression (makethissomethingelse : Caster<_> ) =
    member x.DoIt = makethissomethingelse.Cast // ()

type IDownCastable<'t> =
    abstract member DownCast : 't //= x :> 't
    //interface end

type Foo () =
    inherit IRaw()
    member x.Rawr = "hey"
    
    interface IDownCastable<Foo> with
        member x.DownCast = x
    
type Bar () = 
    inherit IRaw()

    member x.Baz = "yo"
    
    interface IDownCastable<Bar> with
        member x.DownCast = x

// pass the type in a constructor and use that for the conversion??
let b = new Bar()
//let f : obj = b.Raw
let zsz : IDownCastable<Bar> = b.Raw :?> IDownCastable<Bar>



let rawr :_ list = [ box <| new Foo(); box <| new Bar(); ]
let foo = rawr.[1]
let zz : IDownCastable<Bar> = unbox rawr.[1]
zz.DownCast    

// Collection that can put out specific types through child methods
// let arr = d.docs.[1].DOTHECAST
// arr.CustomMethod
//  for v in vals do
    // v.CastTo.GetType()

    
[<Test>]
let ``When trees have matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have no matching content``() = 
    add 2 2 |> should equal 4 

[<Test>]
let ``When trees have partially matching content``() = 
    add 2 2 |> should equal 4 

