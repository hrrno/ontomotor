


// Loading and manipulating Markdown files
// Baseline functionality for file manip to be fed into the TypeProvider

// Load the file

// Grab the props

    // Headers at all levels
        // "incorrect" indentation (ie H4 under H2), should be amalgomated as though it were of an appropriate header
        // eg H1 -> H2 -> H4 & H3 should present like H1 -> H2 -> H3 & H3
        // eg # Foo; ## Bar; #### Baz; ### Zab => Foo.Bar.Baz & Foo.Bar.Zab

    // Loose content 
        // Create an .InnerText prop that lets you dump all content including autoprops
        // Collect all content that isnt in props as .Content (?)
        // Handle missing loose content by returning ""

    // Properties within each section
        // Parse dates, bools, ints, decimal numbers, 

    // List the props detected / the documents schema
        // the files should be able to report on their own structure
            // YAML front matter
            // YAML in subsections?
            // headers and content divisions
        // provide some means of schema validation
            // Doc.MatchesSchema (schema)


open System.IO
open System.Linq
open System.Text.RegularExpressions

let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let testFile = testDir + "content-autoprops-noyaml.md"
let md = File.ReadAllText(testFile)


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
            
type Treez = 
    | Branch of string * list<Treez>
  
type TokenTree = 
    | Node of Token * TokenTree list

type Comparison = | Gt | Lt | Eq
type TokenComparison = Comparison * Token
type TokenComparisonOffset = Comparison * int * Token
type TokenLevels = int * Token

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
        

let rec compareToken (comparisons : TokenComparison list) (tokens : Token list) : TokenComparison list =
    match tokens with
    | x::xs when List.isEmpty comparisons -> compareToken [Eq, x] (x::xs)
    | x::xs::xss -> compareToken (comparisons @ [(compare (x, xs), xs)]) (xs::xss)
    | [_] | []   -> comparisons

let printComparisons (comparisons : TokenComparison list) =
    for (comp, toke) in comparisons do
        printf "%A >> %s\r\n" comp toke.Content

let rec printDomOffset (offset : int) (dom : TokenTree list)  =
    match dom with
    | (Node(token, sub))::xs ->
        let indent = System.String('\t', offset)
        let content = token.Content.Replace("\r\n", "\r\n" + indent)
        printf "%s%s\r\n" indent content
        printDomOffset (offset + 1) sub
        printDomOffset offset xs
    | [] -> ()

let printDom dom = dom |> printDomOffset 0


let headerMatches md = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
let propMatches   md = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
let yamlBlocks    md = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

let captures (matches : MatchCollection) = seq { for m in matches do yield m.Captures.[0] } 

let makeTokens (makeToken : int * string -> Token) matches = 
    matches 
    |> captures 
    |> Seq.map (fun c -> makeToken(c.Index, c.Value.Trim()))
    |> Seq.toList


let headers = md |> headerMatches
let props   = md |> propMatches
let yamls   = md |> yamlBlocks

let document = [Root(0, "Document Root")] 
                 @ makeTokens Header   headers
                 @ makeTokens Property props   
                 @ makeTokens Yaml     yamls 
               |> List.sortBy (fun t -> t.Id)

let comparisons = compareToken [] document


let rec collectSubtrees parentTree comps subTree  =
    match (buildDom comps parentTree) with
    | rest, [] -> rest, subTree
    | (x::rest),  newtrees -> collectSubtrees parentTree rest (subTree @ newtrees) 
    | [] as rest, newtrees -> collectSubtrees parentTree rest (newtrees) 

and buildDom (comparisons : TokenComparison list) (parentTree : TokenTree list)  =
    match comparisons with
    | [] -> [], parentTree
    | (Lt, token)::rest -> 
        let comps, subtree = collectSubtrees parentTree rest []
        comps, (parentTree @ [Node(token, subtree)])
    | (Eq, token)::rest -> 
        let comps, subtree = collectSubtrees [] rest []
        comps, (parentTree @ [Node(token, subtree)])
    | (Gt, token)::rest ->
        let comps, subtree = collectSubtrees [] rest []
        comps, ([Node(token, subtree)])


let extractDom comparisons = buildDom comparisons [] |> snd

let dom = comparisons |> extractDom
comparisons |> printComparisons
dom |> printDom





// decorate the comparisons with an offsett
let offsetCalc comp offset =
    match comp with
    | Lt -> offset - 1
    | Eq -> offset
    | Gt -> offset + 1

let comparisonOffset (comparisons : TokenComparison list) =
    let mutable offset = 0
    [ for (comp, token) in comparisons do
        offset <- offsetCalc comp offset
        yield (comp, offset, token) ]

comparisons |> comparisonOffset

let withNr =  
    comparisons 
    |> List.map (fun (comp, token) -> comp, 0, token )
    |> List.map (fun (comp, off, token) -> comp, (offsetCalc comp 0), token)



let rec compareTokenOffset (comparisons : TokenComparisonOffset list) (tokens : Token list) (offset : int) : TokenComparisonOffset list =
    match tokens with
    | x::xs when List.isEmpty comparisons -> compareTokenOffset [Eq, 0, x] (x::xs) 0
    | x::xs::xss -> 
        let offsetDelta = (offsetCalc (compare (x, xs)) offset)
        compareTokenOffset (comparisons @ [(compare (x, xs), offsetDelta, xs)]) (xs::xss) offsetDelta
    | [_] | []   -> comparisons



let printComparisonsOffset (comparisons : TokenComparisonOffset list) =
    for (comp, offset, toke) in comparisons do
        printf "%A >> %i >> %s\r\n" comp offset toke.Content


let comparisonsOffset = compareTokenOffset [] document 0
comparisonsOffset |> printComparisonsOffset

let levels = comparisonsOffset |> List.map (fun (comp, offset, token) -> (offset, token))




/// Build a tree from elements of 'list' that have larger index than 'offset'. As soon
/// as it finds element below or equal to 'offset', it returns trees found so far
/// together with unprocessed elements.
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
      [Node(n, sub)], rest



let reso = buildTree -1 [] levels

let rec print depth (Node(n, sub)) =
  printfn "%s%s" depth n.Content
  for s in sub do print (depth + "  ") s

reso |> fst |> Seq.head |> print ""








//let lstcountr ls =
//    let rec loop ls total = 
//        match ls with
//        | [] -> total
//        | hd::tl -> loop tl total+1I
//    loop ls 0I
//
//
//let transmorg (comparisons : TokenComparison list) =
//    let rec loop comp total = 
//        match ls with
//        | [] -> total
//        | hd::tl -> loop tl total+1I
//    loop ls 0I




/// Build a tree from elements of 'list' that have larger index than 'offset'. As soon
/// as it finds element below or equal to 'offset', it returns trees found so far
/// together with unprocessed elements.
let rec buildTree2 offset trees list = 
  match list with
  | [] -> trees, [] // No more elements, return trees collected so far
  | (x, _)::xs when x <= offset -> 
      trees, list // The node is below the offset, so we return unprocessed elements
  | (x, n)::xs ->
      /// Collect all subtrees from 'xs' that have index larger than 'x'
      /// (repeatedly call 'buildTree' to find all of them)
      let rec collectSubTrees xs trees = 
        match buildTree2 x [] xs with
        | [], rest -> trees, rest
        | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
      let sub, rest = collectSubTrees xs []
      [Branch(n, sub)], rest




// Trying a new approach: seeding the comparisons with an offset, and using that to govern indentation...

let src = [
        (0, "root");
            (1, "a");
                (2, "a1");
                (2, "a2");
            (1, "b");
                (2, "b1");
                    (3, "b11");
                (2, "b2");
        ]
let res = buildTree2 -1 [] src

/// A helper that nicely prints a tree
let rec print depth (Branch(n, sub)) =
  printfn "%s%s" depth n
  for s in sub do print (depth + "  ") s

res |> fst |> Seq.head |> print ""
    
// props first into Yaml blocks
    // failing that they need to be put in their closest header



// Collect the tokens in ordered fashion:
    // Walk the headers locations
        // walk the property locations, inserting as required
        // walk the YAML blocks, inserting as required



// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...




