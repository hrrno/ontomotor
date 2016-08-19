
#r @"../../Ontomotor/bin/Debug/Ontomotor.dll"
#r @"../../ProtoTypeProvider/bin/Debug/ProtoTypeProvider.dll"
open MarkdownParser
open MarkdownParser.Tokenize


// Åpner metadata mappen
let md = new Proto.TypeProvider.MarkdownProvider<"""C:\proj\ontomotor\src\data\test\test1\""">()



// Strukturert adgang til tilgjengelige filer
for d in md.Docs do
    printfn "%s :: %s" d.Location d.Filename



// Dokument oversikt og strukturert adgang: .Documents, og .Root
let doc = md.Documents.demo_file_2.Root
//let doc = md.Documents.demo_file_2.Root


// Sterk typet data
let definisjon   = doc.Kodeverk.Definisjon
let lagetDato    = definisjon.Laget.ToShortDateString()
let publiserDato = definisjon.Laget


let kvalitet = doc.Kodeverk.Definisjon.Data_kvalitet

let leder = kvalitet.Leder
let dimensjoner = kvalitet.Dimensjoner
