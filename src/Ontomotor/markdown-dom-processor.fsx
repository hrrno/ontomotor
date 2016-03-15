





type Token =  
    | Header   of position : int * content: string 
    | Property of position : int * content: string
with member x.Level = 
        match x with
        | Header (pos,content) -> content.IndexOf("# ") + 1
        | Property _ -> -1
     member x.Content = 
        match x with
        | Header (pos,content) | Property (pos,content) -> content


type Section =
    | Section of Token * children : Section list


type Section2 =
    | Section2 of Token * children : Token list



let looseDoc = 
    [ 
      Header (0, "# h1")
      Header (0, "## h2")
      Header (0, "##### h5")
      Header (0, "###### h6")
      Header (0, "### h3")
      Header (0, "## h2")
      Header (0, "# h1")
      Header (0, "###### h5")
        ]


for d in looseDoc do
    printfn "%i" d.Level



let doc = Section ( Header (0, "# H1"), [] )

// from token list to section list
let rec toSections (tokens: Token list) =
    match tokens with
    | x::xs -> 
        let rec subitems (from:Token list) = 
            seq {
                match from with
                        | f::r when f.Level > x.Level -> 
                            yield Section (f, subitems r)
                        | [f] -> yield Section (f, [])
                        | _ -> ()
            } |> Seq.toList
        let sub = subitems xs
        let remaining = (xs |> List.skip sub.Length)
        [Section (x, sub)] @ toSections remaining
    | [] -> []

looseDoc |> toSections

// from token list to section list
let rec toSectionsN (tokens: Token list) =
    match tokens with
    | x::xs -> 
        let rec subitems (ofToken:Token) (from:Token list) = 
            seq {
                match from with
                        | f::r when f.Level > ofToken.Level -> 
                            yield Section (f, subitems f r)
                        | [f] when f.Level > ofToken.Level -> yield Section (f, [])
                        | _ -> ()
            } |> Seq.toList
        let sub = subitems x xs
        let remaining = (xs |> List.skip (sub.Length))
        [Section (x, sub)] @ toSectionsN remaining
    | [] -> []

looseDoc |> toSectionsN



// from token list to section list
let rec toSectionsX (tokens: Token list) =
    match tokens with
    | x::xs -> 
        let rec subitems (from:Token list) = 
            seq {
                match from with
                        | f::r when f.Level > x.Level -> 
                            yield f
                            yield! subitems r
                        | [f] -> yield f
                        | _ -> ()
            } |> Seq.toList
        let sub = subitems xs

        [Section2 (x, sub)] @ toSectionsX (xs |> List.skip sub.Length)
    | [] -> []

looseDoc |> toSectionsX




let rec buildTree offset trees list = 
  match list with
  | [] -> trees, [] // No more elements, return trees collected so far
  | (x, _)::xs when x <= offset -> 
      trees, list // The node is below the offset, so we return unprocessed elements
  | (x, n)::xs ->
      /// Collect all subtrees from 'xs' that have index larger than 'x'
      /// (repeatedly call 'buildTree' to find all of them)
      let rec collectSubTrees xs trees = 
        match buildTree x [] xs with
        | [], rest -> trees, rest
        | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
      let sub, rest = collectSubTrees xs []
      [Section(n, sub)], rest


let rec print depth (Section(n, sub)) =
  printfn "%s%s" depth n.Content
  for s in sub do print (depth + "  ") s



let res = looseDoc 
            |> List.map (fun x -> x.Level, x) 
            |> buildTree -1 []
            |> fst

res |> Seq.head |> print ""