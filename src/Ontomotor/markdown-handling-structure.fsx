
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
            | Property _ | Yaml _ -> 999

type Comparison = | Gt | Lt | Eq



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
    [
       (Eq, 0, Root   (0,  "Document Root")); 
       (Gt, 1, Header (6,  "#      H1_1"));
       (Gt, 2, Header (21, "##     H1_1_H2_1"));
       (Eq, 2, Header (41, "##     H1_1_H2_2")); 
       (Lt, 1, Header (63, "#      H1_2"));
       (Gt, 2, Header (78, "###    H1_2_H3_1"));
       (Lt, 1, Header (100,"#      H1_3"));
       (Gt, 2, Header (115,"##     H1_3_H2_1"));
       (Gt, 3, Header (135,"####   H1_3_H2_1_H4_1"));
       (Gt, 4, Header (160,"#####  H1_3_H2_1_H4_1_H5_1"));
       (Gt, 5, Header (190,"###### H1_3_H2_1_H4_1_H5_1_H6_1"));
       (Lt, 4, Header (225,"###    H1_3_H2_1_H3_1"))]

let levelled =
  [
   (0, Root   (0,  "Document Root")); 
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
   (3, Header (225,"###    H1_3_H2_1_H3_1"))
  ]

let rawHeader (_,_,h) = h
let withLevel (h:Token) = (h.Level, h)
let headers = doc |> List.map (rawHeader)
let levelledHeaders = doc |> List.map (rawHeader >> withLevel)


//let rec grabChildren offset trees (list : Token list) =
//    match list with
//    | [] -> (trees,[])
//    | x::xs -> 
//        (x, (grabChildren xs))
//    
//
//
//let res = grabChildren 0 [] headers

[ for l in headers do 
    for y in headers do
        yield y
]

