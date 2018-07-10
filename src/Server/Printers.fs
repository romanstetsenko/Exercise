module Printers
open Model.TextAst

module Xml =
    let private tag tag s  = sprintf "<%s>%s</%s>" tag s tag
    let private xmlHeader = """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""

    let prettyPrint (sentences: Sentence list) = 
        let genLevels level = [1..level] |> List.map (fun _ -> "\t") |> String.concat ""
        let wordMap (Word word) = "<word>" + word + "</word>"
        let sentenceMap level (Sentence words) = 
            let lvl = genLevels level
            words |> List.map (fun w ->  genLevels (level + 1)  + wordMap w) |> String.concat "\n"
            |> ( fun ws -> lvl + "<sentence>" + 
                                "\n" + ws + "\n" + 
                                lvl + "</sentence>")

        sentences
        |> List.map (sentenceMap 1)
        |> String.concat "\n"
        |> (fun ss -> "<text>" + "\n" + ss + "\n" + "</text>")            
        |> ( fun txt -> xmlHeader + "\n" + txt)

module Csv =
    let print sentences =
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