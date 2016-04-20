


module MarkdownParser


open System.IO
open System.Text.RegularExpressions


module Tokenize =

    let private title str = (str:string).Trim().Replace(" ", "_")

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
                | Root (p,c) | Header (p,c) | Yaml (p,c) -> c
                | Property (p,c) -> (c.Substring(c.IndexOf(": ") + 2))
             member x.Title = 
                 match x with
                    | Root (p,c) -> c
                    | Header (p,c) -> c.Replace("#", "")                    
                    | Yaml _ -> "YamlBlock"
                    | Property (p,c) -> c.Substring(0, c.IndexOf(": ")) 
                 |> title
             member x.Level = 
                match x with
                | Root _ -> 0
                | Header (p,c) -> c.IndexOf("# ") + 1
                | Yaml _ | Property _ -> -1

                
    type TokenTree = 
        | Node of Token * TokenTree list
        with member x.Token = match x with | Node(n,sub) -> n
             member x.Sub   = match x with | Node(n,sub) -> sub

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

        let calcOffset (preceding:Token, current:Token) currOffset =
            match current with
            | Root _ | Header _ -> current.Level
            | Yaml _ | Property _ ->
                match preceding with
                | Root _ | Header _ -> preceding.Level + 1
                | _ -> currOffset

        let rec levels (comparisons:TokenLevels list) (tokens:Token list) (offset:int) : TokenLevels list =
            match tokens with
            | head::tail when comparisons.IsEmpty -> 
                levels [0, head] (tokens) 0
            | token0::token1::tail -> 
                let newOffset = calcOffset (token0, token1) offset
                levels (comparisons @ [(newOffset, token1)]) (token1::tail) newOffset
            | [_] | []   -> comparisons

        let rec buildTree offset trees list = 
            match list with
            | [] -> trees, [] 
            | (level, _)::tail when level <= offset -> trees, list
            | (level, token)::tail ->
                let rec collectSubTrees xs trees = 
                    match buildTree level [] xs with
                    | [], rest -> trees, rest
                    | newtrees, rest -> collectSubTrees rest (trees @ newtrees)
                let sub, rest = collectSubTrees tail []
                [Node(token, sub)], rest

    let hierarchy tokens = token.levels [] tokens 0

    let toTree hierarchy = token.buildTree -1 [] hierarchy |> fst |> Seq.head

    let tokenTree = hierarchy >> toTree


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
            [ Root(0, "Root") ] 
              @ tokens Header   (md |> headers)
              @ tokens Property (md |> props)   
              @ tokens Yaml     (md |> yamls) 
            |> List.sortBy (fun t -> t.Position)
            |> tokenTree 

    let file path = File.ReadAllText(path) |> markdown
        
