

    // TODO:  and for every `myFoo.Bar` method generate a module function `Foo.Bar a b = (fun (x : Foo) -> x.Bar(a, b))` alternative
//
//type EF = 
//  static member property (expr:Expr<'a -> string>) (cfg:EntityInfo<'a>) = 
//    cfg.Property expr
//  static member property (expr:Expr<'a -> System.DateTime>) (cfg:EntityInfo<'a>) = 
//    cfg.Property expr
//  static member property (expr:Expr<'a -> System.Byte>) (cfg:EntityInfo<'a>) =
//     cfg.Property expr




type EFF = 
    static member Rawr (a:string,b:string) = a
    static member Rawr (a:string,b:string,c:string) = a
    static member Rawr (a:string,b:string,c:int) = a

//    static member Foo (a:string) (b:string) = a
//    static member Foo (a:string) (b:string) (c:string) = c


module Wha =

    type T = T with
        static member ($) (T, n:int  ) = int   (sqrt (float n))
        static member ($) (T, n:int64) = int64 (sqrt (float n))

    let inline sqrtInt (x:'t) :'t = T $ x



module Console =
    module WriteLine' =
        let a = 3
        let b = 3
        let c = 3
        let __ = 4
        let ___ = 4
        let ____ = 4
        let _'' = 4
        let _''' = 4
        let _'''' = 4
        let _''''' = 4
        let o''''' = 4
        let e' = 4
        let e'' = 4
    let WriteLine = 14

    let Write a = 13
    let Write' b c = 14
    let Write'' a b c = 15
    let Write''' a b c d = 4

    let unf<'a> = ""

    let Zoo = 123

module Other =
        
    let s = Console.WriteLine'._'' //.WriteLine'.a
    let x = Console.WriteLine'.___ //.WriteLine'.a
    let c = Console.WriteLine'.____ //.WriteLine'.a
    let d = Console.WriteLine'.o''''' //.WriteLine'.a
    let e = Console.WriteLine'.e' //.WriteLine'.a

    let f = 1 |> Console.Write''' <| 3 <| 4 <| 5

    let w = Console.Write''' 1 2 3 4




type ITesting = 
    interface end





