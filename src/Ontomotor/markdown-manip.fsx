


// Loading and manipulating Markdown files
// Baseline functionality for file manip to be fed into the TypeProvider

// Load the file

// Grab the props

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
open System.Text.RegularExpressions


module Tokenizer =

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

    let rec private printNode depth (Node(n, sub)) =
      printfn "%s%s" depth (n.Content.Replace("\r\n", "\r\n" + depth))
      for s in sub do printNode (depth + "   ") s

    let print (node:TokenTree) = printNode "" node

    type TokenTree with member x.Print = print x


module Lexer =

    open Tokenizer

    type TokenLevels = int * Token

    let private calcOffset (preceding:Token) (current:Token) currOffset =
        match current with
        | Root _ | Header _ -> current.Level
        | Yaml _ | Property _ ->
            match preceding with
            | Root _ | Header _ -> preceding.Level + 1
            | _ -> currOffset

    let rec private tokenLevels (comparisons:TokenLevels list) (tokens:Token list) (offset:int) : TokenLevels list =
        match tokens with
        | x::xs when comparisons.IsEmpty -> 
            tokenLevels [0, x] (x::xs) 0
        | x::xs::xss -> 
            let newOffset = calcOffset x xs offset
            tokenLevels (comparisons @ [(newOffset, xs)]) (xs::xss) newOffset
        | [_] | []   -> comparisons

    let rec private buildTree offset trees list = 
      match list with
      | [] -> trees, [] 
      | (level, _)::xs when level <= offset -> trees, list
      | (level, token)::xs ->
          let rec collectSubTrees xs trees = 
            match buildTree level [] xs with
            | [], rest -> trees, rest
            | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
          let sub, rest = collectSubTrees xs []
          [Node(token, sub)], rest

    let hierarchy tokens = tokenLevels [] tokens 0

    let toTree hierarchy = buildTree -1 [] hierarchy |> fst |> Seq.head


module Parser =

    open Tokenizer
    open Lexer

    let private captures (matches : MatchCollection) = seq { for m in matches do yield m.Captures.[0] } 

    let private makeTokens (makeToken : int * string -> Token) matches = 
        matches 
        |> captures 
        |> Seq.map (fun c -> makeToken(c.Index, c.Value.Trim()))
        |> Seq.toList

    let private headerMatches md = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
    let private propMatches   md = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
    let private yamlBlocks    md = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

    let parse md =
            [ Root(0, "Document Root") ] 
              @ makeTokens Header   (md |> headerMatches)
              @ makeTokens Property (md |> propMatches)   
              @ makeTokens Yaml     (md |> yamlBlocks) 
            |> List.sortBy (fun t -> t.Position)
            |> hierarchy 
            |> toTree

    let parseFile path = parse <| File.ReadAllText(path)
        

let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let reso = Parser.parseFile(testDir + "content-autoprops-badstructure.md")

reso.Print 


// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...

