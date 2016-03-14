


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
            
type Tree = 
    | Branch of string * list<Tree>
  
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
        

let rec compareToken (comparisons : TokenComparison list) (tokens : Token list) : TokenComparison list =
    match tokens with
    | x::xs when List.isEmpty comparisons -> compareToken [Lt, x] (x::xs)
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
    | [] as rest, newtrees -> collectSubtrees parentTree rest (subTree @ newtrees) 

and buildDom (comparisons : TokenComparison list) (parentTree : TokenTree list)  =
    match comparisons with
    | [] -> [], parentTree
    | (Lt, token)::rest -> 
        let comps, subtree = collectSubtrees parentTree rest []
        comps, ([Node(token, subtree)] @ parentTree)
    | (Eq, token)::rest -> 
        let comps, subtree = collectSubtrees parentTree rest []
        comps, (parentTree @ [Node(token, subtree)])
    | (Gt, token)::rest ->
        let comps, subtree = collectSubtrees parentTree rest []
        comps, ([Node(token, [])] @ subtree)


let extractDom comparisons = buildDom comparisons [] |> snd

let dom = comparisons |> extractDom
comparisons |> printComparisons
dom |> printDom




    
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




