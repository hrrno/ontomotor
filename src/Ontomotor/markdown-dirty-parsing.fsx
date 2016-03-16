



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
         member x.Level = 
            match x with
            | Root _ -> 0
            | Header (pos,content) -> content.IndexOf("# ") + 1
            | Property _ | Yaml _ -> -1

            
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
        | Header _ -> 
            match second with
            | second when first.Level < second.Level -> Gt
            | second when first.Level > second.Level -> Lt
            | _ -> Eq
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



let doc =
    [  (Eq, 0, Root (0,"Document Root")); 
       (Gt, 1, Header (6,"#      H1_1"));
       (Gt, 2, Header (21,"##     H1_1_H2_1"));
       (Eq, 2, Header (41,"##     H1_1_H2_2")); 
       (Lt, 1, Header (63,"#      H1_2"));
       (Gt, 2, Header (78,"###    H1_2_H3_1"));
       (Lt, 1, Header (100,"#      H1_3"));
       (Gt, 2, Header (115,"##     H1_3_H2_1"));
       (Gt, 3, Header (135,"####   H1_3_H2_1_H4_1"));
       (Gt, 4, Header (160,"#####  H1_3_H2_1_H4_1_H5_1"));
       (Gt, 5, Header (190,"###### H1_3_H2_1_H4_1_H5_1_H6_1"));
       (Lt, 4, Header (225,"###    H1_3_H2_1_H3_1"))]


let levelled =
  [(0, Root   (0,  "Document Root")); 
   (1, Header (6,  "#      H1_1"));
   (2, Header (21, "##     H1_1_H2_1")); 
   (2, Header (41, "##     H1_1_H2_2"));
   (1, Header (63, "#      H1_2")); 
   (3, Header (78, "###    H1_2_H3_1"));
   (1, Header (100,"#      H1_3")); 
   (2, Header (115,"##     H1_3_H2_1"));
   (4, Header (135,"####   H1_3_H2_1_H4_1"));
   (5, Header (160,"#####  H1_3_H2_1_H4_1_H5_1"));
   (6, Header (190,"###### H1_3_H2_1_H4_1_H5_1_H6_1"));
   (3, Header (225,"###    H1_3_H2_1_H3_1"))]


let level (_,_,h:Token) = h.Level, h
let tokensOut (_,_,h) = h
let levelized = doc |> List.map level
let tokens = doc |> List.map tokensOut

(*
        public static void Main(string[] args)
        {
            Heading[] headings = new[] {
                new Heading { Level = 1, Text = “Intro” },
                new Heading { Level = 2, Text = “Hello” },
                new Heading { Level = 2, Text = “Summary” },
                new Heading { Level = 1, Text = “Technical Info” },
                new Heading { Level = 2, Text = “Class API” },
                new Heading { Level = 3, Text = “Class1″ },
                new Heading { Level = 3, Text = “Class2″ },
                new Heading { Level = 2, Text = “Interoperability” },
                new Heading { Level = 3, Text = “Scenario 1″ },
                new Heading { Level = 3, Text = “Scenario 2″ },
                new Heading { Level = 1, Text = “In Conclusion” }   };

 
        public static IEnumerable<XElement> GetChildrenHeadings(
            IEnumerable<Heading> headingList,
            Heading parent)
        {
            return
                headingList
                    .SkipWhile(h => h != parent)
                    .Skip(1)
                    .TakeWhile(h => h.Level > parent.Level)
                    .Where(h => h.Level == parent.Level + 1)
                    .Select(h =>
                        new XElement(“Heading” + h.Level,
                            new XAttribute(“Name”, h.Text),
                            GetChildrenHeadings(headingList, h)
                        )
                    );
        }
            var toc = new XElement(“Root”,
                    headings
                        .Where(h => h.Level == 1)
                        .Select
                        (
                            h => new XElement(“Heading” + h.Level,
                                new XAttribute(“Name”, h.Text),
                                GetChildrenHeadings(headings, h)
                            )
                        )
                ); Console.WriteLine(toc); } }
*)



//query {
//        for student in db.Student do
//        groupBy student.Age into g        
//        let total = query { for student in g do sumByNullable student.Age }
//        select (g.Key, g.Count(), total)
//}



levelized



let src = [
        (0, "root");
            (1, "a");
                (2, "a1");
                (3, "a1");
            (1, "b");
                (2, "b1");
                    (3, "b11");
            (1, "c1");
        ]

let srcL = [
        (0, "root");
            (1, "a");
                (2, "a1");
                (2, "a1");
            (1, "b");
                (3, "b1");
            (1, "c");
                (2, "c2");
                    (4, "c3");
                        (5, "c4");
                            (6, "c5");
                    (3, "c6");
        ]

type Tree = 
  | Branch of string * list<Tree>


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

let res = buildTree -1 [] srcL

/// A helper that nicely prints a tree
let rec print depth (Branch(n, sub)) =
  printfn "%s%s" depth n
  for s in sub do print (depth + "  ") s

res |> fst |> Seq.head |> print ""