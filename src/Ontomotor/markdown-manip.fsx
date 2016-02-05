
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
open System.Text.RegularExpressions

let testDir = __SOURCE_DIRECTORY__ + "/../data/test/test1/"
let testFile = testDir + "content-autoprops.md"
let md = File.ReadAllText(testFile)

// yaml blocks, headers, subsection props

// Tokenize?

type MarkdownContent = 
    | YamlBlock of int * string 
    | Header of int * string


let headerMatches = Regex.Matches(md, "^(#+)(.*)$",   RegexOptions.Multiline)
let propMatches   = Regex.Matches(md, "^(\w+:)(.*)$", RegexOptions.Multiline)
let yamlBlocks    = Regex.Matches(md, "---(.*?)---",  RegexOptions.Singleline)

for m in yamlBlocks do
    printf "%s\r\n" (m.Captures.[0].Value.ToString()) // .Index.ToString())






// Load files as a collection
    // Provide a unified type for all files in the collection for loops
    // Perhaps add a downcast operator to get the raw, unrestricted, manifestation of the file?
        // ie GroupObj.Explicit runs the provider on a single file, with only those props...




