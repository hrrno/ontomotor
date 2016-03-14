
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
let testFile = testDir + "content-autoprops-simple.md"
let md = File.ReadAllText(testFile)

// Tokenize?

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

let captures (matches : MatchCollection) = seq { for m in matches do yield m.Captures.[0] } 

let makeTokens (token : int * string -> Token ) matches  = 
    matches 
    |> captures 
    |> Seq.map (fun c -> token(c.Index, c.Value.Trim()))
    |> Seq.toList

let headerMatches = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
let propMatches   = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
let yamlBlocks    = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

let combined = [Root(0, "Document Root")] 
                 @ makeTokens Header   headerMatches 
                 @ makeTokens Yaml     yamlBlocks 
                 @ makeTokens Property propMatches 
               |> List.sortBy (fun t -> t.Id)


//type TokenAcc = TokenAcc of main : Token list * child : Token list
//    with member this.Latest = 
//                match this with 
//                | TokenAcc (main, child) -> 
//                    match child with
//                    | x :: xs -> x
//                    | [] -> Blank

// delete this...
type Tree = 
  | Branch of string * list<Tree>

let src = [
            (0, "root");
                (1, "a");
                    (2, "a1");
                    (2, "a2");
                (1, "b");
                    (2, "b1");
                        (3, "b11");
                    (1, "b2");
            ]


type TokenTree = 
    | Node of Token * TokenTree list

type Comparison = | Gt | Lt | Eq
type TokenComparison = Comparison * Token

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
    //| _ -> Lt


let rec checkToken (comparisons : TokenComparison list) (tokens : Token list) : TokenComparison list =
    match tokens with
    | x::xs when List.isEmpty comparisons -> checkToken [Eq, x] (x::xs)
    | x::xs::xss -> checkToken (comparisons @ [(compare (x, xs), xs)]) (xs::xss)
    | [_] | []   -> comparisons
    
let comparisons = checkToken [] combined

for (comp, toke) in comparisons do
    printf "%A >> %s\r\n" comp toke.Content

let mutable i = 0
let rec buildDom (comparisons : TokenComparison list) (trees : TokenTree list)  =
    i <- i + 1
    printf "%i BUILDING THE DOM\r\n" i
    match comparisons with
    | [] -> [], trees
    | (Lt, _)::xs -> xs, trees
    | (comp, token)::xs ->
        let rec collectSubtrees remainingComparisons tree =
            i <- i + 1
            printf "%i RECURSING ON THE DOM\r\n" i
            match (buildDom remainingComparisons [ Node (token, []) ]) with
            | rest, [] -> rest, tree
            | (x::rest), newtrees -> collectSubtrees rest (tree @ newtrees)
            | [], newtrees -> collectSubtrees [] (tree @ newtrees)
        let sub, rest = collectSubtrees xs []
        let node = Node(token, rest)
        sub, [node]




let dom = buildDom comparisons []


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
      [Branch(n, sub)], rest

let res = buildTree -1 [] src

// Run up the list backwards, pushing content onto its parent?
    // Check if each node is @less important@ than the node it meets going up
        // if it is then it gets added as child content
        // if it isnt then it gets setup as a new child content group
            // parent group gets merged into a main content accumulator?
    // push all top level headers to the document root (and/or define the root as a level 0 construct)






    
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




