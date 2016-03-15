


(* 

Eq >> 0 >> Document Root
Gt >> 1 >> # Foo
Eq >> 1 >> ## Bar
Eq >> 1 >> ## Beans
Eq >> 1 >> ## Boo
Gt >> 2 >> Baz: There
Lt >> 1 >> ## SubObject
Gt >> 2 >> Autoprop: This is an autoprop
Eq >> 2 >> Boolprop: true
Eq >> 2 >> Dateprop: 2011-10-10


*)



type Token = 
    | Root     of position : int * content: string 
    | Header   of position : int * content: string 
    | Yaml     of position : int * content: string
    | Property of position : int * content: string
    //| Blank
    with member x.Id = 
            match x with
            | Root     (i,c) -> i
            | Header   (i,c) -> i
            | Yaml     (i,c) -> i
            | Property (i,c) -> i
            //| Blank          -> -1
         member x.Content = 
            match x with
            | Root     (i,c) -> c
            | Header   (i,c) -> c
            | Yaml     (i,c) -> c
            | Property (i,c) -> c
            //| Blank          -> -1


type Comparison = | Gt | Lt | Eq
let compare (first, second) =
    match first with
    | Root _ -> Gt
    | Header _ -> 
        match second with 
        | Header _ -> Eq
        | _ -> Gt
    | Yaml _ -> 
        match second with 
        | Header _ -> Lt
        | Yaml _ | Property _ -> Eq
        | _ -> Lt
    | Property _ ->
        match second with 
        | Yaml _ | Property _ -> Eq
        | _ -> Lt
        
                    

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq


module Tree = 

    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = cata fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = fold fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the subitems using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum 
            // ... and return it
            finalAccum 

    let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = map fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = subtrees |> Seq.map recurse 
            InternalNode (newNodeInfo, newSubtrees)



(*
let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.fileSize
    let fDir acc (dir:DirectoryInfo)= 
        acc + dir.dirSize
    Tree.fold fFile fDir 0 fileSystemItem 
*)

type FileInfo = {name:string; fileSize:int;Length:int}
type DirectoryInfo = {name:string; dirSize:int; }

type FileSystemItem = Tree<FileInfo,DirectoryInfo>

type Section = {Title:string}
type Property = {Name:string;Content:string }

type MdDom = Tree<Property,Section>

let fromProperty (contentInfo:Property) = MdDom.LeafNode contentInfo
let fromSection (sectionInfo:Section) subitems = MdDom.InternalNode (sectionInfo, subitems)


let h2Content = fromProperty {Name="Prop"; Content="there"}

let h2 = fromSection {Title="#Bar"} [h2Content]
let h1 = fromSection {Title="#Foo"} [h2]
let root = fromSection {Title="Document Root"} [h1]


let document = 
         [ Root (0,"Document Root"); 
           Header (2,"# Foo"); 
           Header (11,"## Bar");
           Header (21,"## Beans"); 
           Header (33,"## Boo"); 
                Property (51,"Baz: There");
           Header (65,"## SubObject"); 
                Property (161,"Autoprop: This is an autoprop");
                Property (194,"Boolprop: true"); 
                Property (212,"Dateprop: 2011-10-10")]

document |> List.rev

//document |> foldInProperties

// If youre a header and I`m a prop, I`m yours
// If I`m a header and you`re a header, I`m yours if smaller, your parents if not

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.Length
    let fDir acc (dir:DirectoryInfo)= 
        acc 
    Tree.fold fFile fDir 0 fileSystemItem 

open System.IO


//let fromToken token = 
//    match token with
//    | Root (i,c) | Header (i,c) -> fromSection { Title = c }
//    | Yaml (i,c) | Property (i,c) -> fromProperty { Name = c; Content = c  }

let propFromContent content =
    fromProperty { Name = content; Content=content; }

//
//let rec readChildrenss (document:Token list) =
//    let subItems = seq {
//        for token in document do
//            match token with
//            | Root (i,c) | Header (i,c) -> yield! readChildren (propFromContent c)
//            | Yaml (i,c) | Property (i,c) -> yield! fromProperty { Name = c; Content = c  }
//        }
//    MdDom.InternalNode ({Title="Root"}, subItems )




let fromFile (fileInfo:FileInfo) = 
    LeafNode fileInfo 

let rec fromDir (dirInfo:DirectoryInfo) = 
    let subItems = seq{
        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
        }
    InternalNode (dirInfo,subItems)

    

let rec readChildren (document:Token list) =
    match document with
    | [] -> failwith  "nooooo"
//        printfn "ahoy sailpor\r\n"
//        MdDom.InternalNode ({Title="Root"}, [] )
    | [lastToken] -> failwith  "huh"
    | token::xs -> 
        let rec subItems = seq {
            //yield // MdDom.LeafNode ({Name=token.Content; Content=token.Content}) 
            
            match token with
                //| Root (i,c) | Header (i,c) -> yield readChildren xs
                
                | Yaml (i,c) | Property (i,c) -> 
                    //yield readChildren xs
                    yield fromProperty { Name = c; Content = c  }
                | _ -> ()
            yield readChildren xs                    
        }
        MdDom.InternalNode ({Title=token.Content}, subItems)
            

let foo = document |> readChildren




