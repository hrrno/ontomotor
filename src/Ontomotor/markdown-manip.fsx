


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

type Token = 
    | Root     of position : int * content: string 
    | Header   of position : int * content: string 
    | Yaml     of position : int * content: string
    | Property of position : int * content: string
    with member x.Position = 
            match x with
            | Root (i,c) | Header (i,c) 
            | Yaml (i,c) | Property (i,c) -> i
         member x.Content = 
            match x with
            | Root (i,c) | Header (i,c) 
            | Yaml (i,c) | Property (i,c) -> c
         member x.Level = 
            match x with
            | Root _ -> 0
            | Header (pos,content) -> content.IndexOf("# ") + 1
            | Property _ | Yaml _ -> -1

type TokenTree = 
    | Node of Token * TokenTree list

type TokenLevels = int * Token

let rec printNode depth (Node(n, sub)) =
  printfn "%s%s" depth (n.Content.Replace("\r\n", "\r\n" + depth))
  for s in sub do printNode (depth + "   ") s

let print (node:TokenTree) = printNode "" node

let headerMatches md = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
let propMatches   md = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
let yamlBlocks    md = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

let captures (matches : MatchCollection) = seq { for m in matches do yield m.Captures.[0] } 

let makeTokens (makeToken : int * string -> Token) matches = 
    matches 
    |> captures 
    |> Seq.map (fun c -> makeToken(c.Index, c.Value.Trim()))
    |> Seq.toList


let newLvlCalc (parent:Token) (maybeChild:Token) currOffset =
    match maybeChild with
    | Root _ | Header _ -> maybeChild.Level
    | Yaml _ | Property _ ->
        match parent with
        | Root _ | Header _ -> parent.Level + 1
        | _ -> currOffset

let rec tokenLevels (comparisons : TokenLevels list) (tokens : Token list) (offset : int) : TokenLevels list =
    match tokens with
    | x::xs when comparisons.IsEmpty -> tokenLevels [0, x] (x::xs) 0
    | x::xs::xss -> 
        let newOffset = newLvlCalc x xs offset
        tokenLevels (comparisons @ [(newOffset, xs)]) (xs::xss) newOffset
    | [_] | []   -> comparisons

let hierarchy document = tokenLevels [] document 0

let rec buildTree offset trees list = 
  match list with
  | [] -> trees, [] 
  | (x, _)::xs when x <= offset -> trees, list
  | (x, n)::xs ->
      let rec collectSubTrees xs trees = 
        match buildTree x [] xs with
        | [], rest -> trees, rest
        | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
      let sub, rest = collectSubTrees xs []
      [Node(n, sub)], rest

let toTree hierarchy = buildTree -1 [] hierarchy


let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let testFile = testDir + "content-autoprops-tricky.md"
let md = File.ReadAllText(testFile)

let headers = md |> headerMatches
let props   = md |> propMatches
let yamls   = md |> yamlBlocks

let document = [Root(0, "Document Root")] 
                 @ makeTokens Header   headers
                 @ makeTokens Property props   
                 @ makeTokens Yaml     yamls 
               |> List.sortBy (fun t -> t.Position)


let reso = document |> hierarchy |> toTree

reso |> fst |> Seq.head |> print



// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...




