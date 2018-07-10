#if !INTERACTIVE
module Model
#endif

#if INTERACTIVE
#r "System.Core.dll"
#r "System.Xml.Linq" 
#endif

module TextAst =
    type Word = | Word of string
    type Sentence = | Sentence of Word list
    type Text = | Text of Sentence list 

    let sortSentences (Text xs) =
        let sentenceFolder acc (Sentence sentence) =
            let toLower (x:string) = x.ToLower() 
            let s = sentence 
                    |> List.sortBy ( fun (Word w) -> toLower w)
                    |> Sentence
            acc @ [s]
        xs
        |> List.fold sentenceFolder []

module Converters =
    open TextAst

    module Xml =
        let private tag tag s  = sprintf "<%s>%s</%s>" tag s tag
        let private xmlHeader = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""

        let convert sentences = 
            let render tagName innerRenderer list = 
                list
                |> List.map innerRenderer
                |> String.concat ""
                |> tag tagName

            let renderWord = fun (Word w) -> render "word" id [w] 
            let renderSentence = fun (Sentence s) -> render "sentence" renderWord s
            let renderText = fun sentences -> render "text" renderSentence sentences
            xmlHeader + (sentences |> renderText ) 
    
    module Csv =
        let convert sentences =
            let delimiter = ", "
            let genHeaderCells i =
                let dummyCell = ""
                let headerCells = 
                    [1..i]
                    |> List.map (sprintf "Word %i") 
                dummyCell::headerCells
            
            let genHeader ss =                 
                let longestSentence acc (Sentence s) =
                    let currentLength = List.length s 
                    if acc <= currentLength then currentLength else acc 
                let i = List.fold longestSentence 0 ss
                genHeaderCells i |> String.concat delimiter
 
            let toCsvRows sentences =
                let toCsvRow index (Sentence ws) =
                    let cells = ws |> List.map (fun (Word w) -> w)
                    let rh = sprintf "Sentence %i" (index + 1)
                    rh::cells |> String.concat delimiter
                let body = sentences |> List.mapi toCsvRow
                let header = genHeader sentences
                header::body

            sentences |> toCsvRows |> String.concat "\n"

open TextAst
let text = 
    [["A"; "B"];["C"; "D"];["Z"; "F"]; ["y"; "G"]; ["X"; "h"]]
    |> List.map 
        (fun sen -> 
            sen
            |> List.map Word
            |> Sentence)
    |> Text 


text |> printfn "%A"    
//sortText text |> printfn "%A"
//sortText text |> Mappers.toXml |> printfn "%A"
//sortText text |> Mappers.toXml |> XmlAst.toCompactString |> printfn "%A"
//sortText text |> Mappers.toXDocument |> Result.bind (XDocAst.toString) |> printfn "%A"

// open System
// type UseCase_v1 = 
//     String -> (* plain input text*)
//         ParsedText -> (* parsed and structured input text *)
//             ParsedText -> (* now with sorted words *)
//                 string (*rendered in CSV or XML formats*) 
// type UseCase_v1_a = 
//     String -> (* plain input text*)
//         ParsedText -> (* parsed and structured input text *)
//             SortedText -> (* a structure which is isomorphic to the Text structure *) 
//                 string (* rendered in CSV or XML formats *) 

// type UseCase_v2 =
//     String -> (* plain input text*)
//         ParsedText ->  (* parsed and structured input text *)
//             ParsedText -> (* now with sorted words *)
//                 OutputFormat -> (* CSV or XML strutures, which are also isomorphic to the Parsedtext *)
//                     string (* the output structure rendered*)

// type UseCase_v2_a =
//     String -> (* plain input text*)
//         ParsedText -> (* parsed and structured input text *)
//             SortedText -> (* a structure which is isomorphic to the Text structure *)
//                 OutputFormat -> (* CSV or XML strutures, which are also isomorphic to the Parsedtext*)
//                     string (* the output structure rendered*)
