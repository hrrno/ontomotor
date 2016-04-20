
module MarkdownStructure


// TODO: figure out how to handle conflicting data types ie IOne.Prop : int vs ITwo.Prop : string [(an exception?)]


module Interface =         
    
    open MarkdownParser.Tokenize

    type IItem = { Name : string; }

    type ITree = 
        | IFace of IItem * ITree list
        | IProp of IItem
        with member x.Item = match x with | IFace(n,sub) -> n | IProp (n) -> n
             member x.Sub  = match x with | IFace(n,sub) -> sub | IProp _ -> []
             member x.Props = x.Sub |> List.filter (function | IProp _ -> true | _ -> false)
             member x.Faces = x.Sub |> List.filter (function | IFace _ -> true | _ -> false)

    let rec print (tree:ITree) =
        printfn "interface: %s = %s" tree.Item.Name tree.Item.Name
        match tree with
        | IProp _ -> ()
        | IFace (item, sub) -> for s in sub do print s

    [<AutoOpen>]
    module private Merging =
     
        let props item = match item with | IProp _ -> true | _ -> false
        let justProps item = (item:ITree).Sub |> List.filter props 
        let propSet item = item |> justProps |> set
        let isPropertySubsetOf s2 s1 = Set.isSubset (s1 |> propSet) (s2 |> propSet)
        let isPropertyMatchedWith s2 s1 = s1 |> isPropertySubsetOf s2 || s2 |> isPropertySubsetOf s1
        let isInterfaceSubsetOf l2 l1 = Set.isSubset (set (l1:ITree).Sub) (set (l2:ITree).Sub)
        let interfacesWithProperties = function | IFace (n, []) -> false | _ -> true
        let face (name, subs) = IFace ({ Name = name }, subs)
        let mergedFace subs = ("IShared", subs) |> face
        let emptyFace = ("IAmEmpty", []) |> face

        let (|InterfaceSubSet|_|)   (lhs, rhs) = if lhs |> isInterfaceSubsetOf   rhs then Some (lhs, rhs) else None
        let (|InterfaceSuperSet|_|) (lhs, rhs) = if rhs |> isInterfaceSubsetOf   lhs then Some (lhs, rhs) else None
        let (|PropertyMatch|_|)     (lhs, rhs) = if lhs |> isPropertyMatchedWith rhs then Some (lhs, rhs) else None

        let mutable accumulatorSeed : Set<ITree> ref = ref ([] |> Set.ofList)
        let withSubtrees s = !(s:Set<ITree> ref) |> Set.filter (fun item -> not (item.Sub.IsEmpty))
        let removeFrom s item = (s:Set<ITree> ref) := (!s).Remove item
        let addTo s item = (!(s:Set<ITree> ref)).Add item
        let toList s = !s |> Set.toList

        let rec deepMerge (lhs : ITree) (rhs : ITree) : ITree = 
            let mergedSubFace = lhs.Faces @ rhs.Faces |> List.fold deepMerge emptyFace

            [mergedSubFace] @ lhs.Props @ rhs.Props 
            |> List.filter interfacesWithProperties
            |> set 
            |> Set.toList
            |> mergedFace

        let merge (interfaces : Set<ITree> ref) (rhs : ITree) (lhs : ITree)  = 
            match (lhs, rhs) with
            | InterfaceSuperSet _ -> 
                lhs
            | InterfaceSubSet _ -> 
                lhs |> removeFrom interfaces
                rhs
            | PropertyMatch _ ->
                lhs |> removeFrom interfaces
                deepMerge rhs lhs
            | _ -> rhs
            
        let rec mergedTree (Node(token, subTokens) : TokenTree) =

            let extractInterface (node:TokenTree) = 
                match node.Token with 
                | Header _   | Root _ -> ("IShared", node |> mergedTree) |> face
                | Property _ | Yaml _ -> IProp({ Name = node.Token.Title }) 

            let mergedWith interfaces node =
                interfaces 
                |> withSubtrees
                |> Set.fold (merge interfaces) (node |> extractInterface)

            let mergeAll (interfaces : Set<ITree> ref) node =
                node 
                |> mergedWith interfaces
                |> addTo interfaces
                |> ref

            match subTokens with 
            | [] -> [ emptyFace ]
            | _ -> 
                   subTokens
                   |> List.fold mergeAll accumulatorSeed  
                   |> toList

    let tree (tokens:TokenTree) = tokens |> mergedTree |> List.head