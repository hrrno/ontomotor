


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


type FileInfo = {name:string; fileSize:int}
type DirectoryInfo = {name:string; dirSize:int}

type FileSystemItem = Tree<FileInfo,DirectoryInfo>


type Section = { Depth:int; Title:string }
type Content = {Type:string;Content:string }

type MdDom = Tree<Content,Section>

let fromSection (sectionInfo:Section) subitems = InternalNode (sectionInfo, subitems)
let fromSection (contentInfo:Content) = LeafNode contentInfo


