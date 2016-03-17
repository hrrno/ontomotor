


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


module Tokenize =

    type Token = 
        | Root     of position : int * content: string 
        | Header   of position : int * content: string 
        | Yaml     of position : int * content: string
        | Property of position : int * content: string
        with member x.Position = 
                match x with
                | Root (p,c) | Header   (p,c) 
                | Yaml (p,c) | Property (p,c) -> p
             member x.Content = 
                match x with
                | Root (p,c) | Header   (p,c) 
                | Yaml (p,c) | Property (p,c) -> c
             member x.Level = 
                match x with
                | Root _ -> 0
                | Header (pos,content) -> content.IndexOf("# ") + 1
                | Property _ | Yaml _ -> -1

    type TokenTree = 
        | Node of Token * TokenTree list

    module private tree =    

        let rec printNode depth (Node(n, sub)) =
          printfn "%s%s" depth (n.Content.Replace("\r\n", "\r\n" + depth))
          for s in sub do printNode (depth + "   ") s

        let print (node:TokenTree) = printNode "" node

    type TokenTree with member x.Print = tree.print x

    let captures (matches : MatchCollection) = seq { for m in matches do yield m.Captures.[0] } 

    let tokens (makeToken : int * string -> Token) matches = 
        matches 
        |> captures 
        |> Seq.map (fun c -> makeToken(c.Index, c.Value.Trim()))
        |> Seq.toList


module Lex =

    open Tokenize

    module private token =

        type TokenLevels = int * Token

        let calcOffset (preceding:Token) (current:Token) currOffset =
            match current with
            | Root _ | Header _ -> current.Level
            | Yaml _ | Property _ ->
                match preceding with
                | Root _ | Header _ -> preceding.Level + 1
                | _ -> currOffset

        let rec levels (comparisons:TokenLevels list) (tokens:Token list) (offset:int) : TokenLevels list =
            match tokens with
            | x::xs when comparisons.IsEmpty -> 
                levels [0, x] (x::xs) 0
            | x::xs::xss -> 
                let newOffset = calcOffset x xs offset
                levels (comparisons @ [(newOffset, xs)]) (xs::xss) newOffset
            | [_] | []   -> comparisons

        let rec buildTree offset trees list = 
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

    let hierarchy tokens = token.levels [] tokens 0

    let toTree hierarchy = token.buildTree -1 [] hierarchy |> fst |> Seq.head


module Parse =

    open Tokenize
    open Lex
    
    module private parser =

        let headers md = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
        let props   md = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
        let yamls   md = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

    open parser
    
    type Markdown = string

    let markdown (markdown as md:Markdown) =
            [ Root(0, "Document Root") ] 
              @ tokens Header   (md |> headers)
              @ tokens Property (md |> props)   
              @ tokens Yaml     (md |> yamls) 
            |> List.sortBy (fun t -> t.Position)
            |> hierarchy 
            |> toTree

    let file path = File.ReadAllText(path) |> markdown
        

let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let reso = testDir + "content-autoprops-badstructure.md" |> Parse.file

reso.Print 


// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...

