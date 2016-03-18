


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


#load "MarkdownParser.fs"
open MarkdownParser

let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let reso = testDir + "content-autoprops-badstructure.md" |> MarkdownParser.Parse.file


reso.Print 



// Run through the properties and spit out  

let rec propGen (tree:Tokenize.TokenTree list) =
    for Tokenize.Node (toke, subtree) in tree do
        printf "%s" toke.Content
        propGen subtree 

propGen [ reso ]


// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...

