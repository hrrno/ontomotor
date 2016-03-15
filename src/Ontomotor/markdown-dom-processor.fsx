





type Token =  
    | Header   of position : int * content: string 
    | Property of position : int * content: string
with member x.Level = 
        match x with
        | Header (pos,content) -> content.IndexOf("# ") + 1
        | Property _ -> -1


type Section =
    | Section of Token * children : Section list



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