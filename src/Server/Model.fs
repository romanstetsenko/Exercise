#if !INTERACTIVE
module Model
#endif

#if INTERACTIVE
#r "System.Core.dll"
#r "System.Xml.Linq" 
#endif

module TextAst =
    type Word = | Word of string

    type InputSentence = | InputSentence of Word list
    type InputText = | InputText of InputSentence list

    type OutputSentence = | OutputSentence of Word list
    type OutputText = | OutputText of OutputSentence list
    
    let sortCaseInsensetive (InputText xs) =
        let sentenceFolder acc (InputSentence sentence) =
            let toLower (x:string) = x.ToLower() 
            let s = sentence 
                    |> List.sortBy ( fun (Word w) -> toLower w)
                    |> OutputSentence
            acc @ [s]
        xs
        |> List.fold sentenceFolder [] 
        |> OutputText

module XmlAst =
    type XmlWord = | XmlWord of string 
    type XmlSentence = | XmlSentence of XmlWord list 
    type XmlHeader = | XmlHeader of string
    type XmlText = | XmlText of XmlSentence list 
    type Xml = | Xml of XmlHeader * XmlText
    let private tag tag s  = sprintf "<%s>%s</%s>" tag s tag

    let toCompactString (Xml ((XmlHeader header), text)): string =
        let render tagName innerRenderer list = 
            list
            |> List.map innerRenderer
            |> String.concat ""
            |> tag tagName

        let renderWord = fun (XmlWord xw) -> render "word" id [xw] 
        let renderSentence = fun (XmlSentence xs) -> render "sentence" renderWord xs
        let renderText = fun (XmlText xt) -> render "text" renderSentence xt
        
        header + (text |> renderText )

module XDocAst =
    open System.Xml.Linq
    
    let XDeclaration version encoding standalone = XDeclaration(version, encoding, standalone)
    let XName expandedName = XName.Get(expandedName)
    let XDocument xdecl content = XDocument(xdecl, content |> Seq.map (fun v -> v :> obj) |> Seq.toArray)
    let XElement expandedName content = XElement(XName expandedName, content |> Seq.map (fun v -> v :> obj) |> Seq.toArray) :> obj
    open System.IO
    let toString (xDoc:XDocument) =
        try
            use wr = new StringWriter()
            xDoc.Save(wr)
            Result.Ok (wr.ToString()) 
        with ex -> Result.Error (ex.ToString())

module CsvAst = 

    type Cell =
        | DummyCell 
        | Cell of string 

    type Row = | Row of Cell list
    
    type Csv = | Csv of Row list
    let toString (Csv rows) = 
        let renderCell = function
            | DummyCell -> ""
            | Cell c -> c
        rows
        |> List.map ( fun (Row r) -> r |> List.map renderCell |> String.concat ", ")
        |> String.concat "\n" 
        
module Mappers =
    open TextAst

    open XmlAst
    let toXml (OutputText t) =
        let toXmlHeader () = XmlHeader """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""   
        let toXmlWord (Word w) = XmlWord w
        let toXmlSentence (OutputSentence s) = s |> List.map toXmlWord |> XmlSentence    
        let toXmlText ss =  ss |> List.map toXmlSentence |> XmlText
        let text = toXmlText t
        let header = toXmlHeader ()
        Xml (header, text)
    
    open XDocAst
    let toXDocument (OutputText t) =
        let toXWord (Word w) = XElement "word" [w]
        let toXWords ws = ws |> List.map toXWord
        let toXSentence (OutputSentence s) = XElement "sentence" (s |> toXWords )
        let xText = XElement "text" (t |> List.map toXSentence )
        try
            let xDoc = XDocument (XDeclaration "1.0" "UTF-8" "yes") [xText]
            Result.Ok xDoc
        with ex -> Result.Error (ex.ToString())
    
    open CsvAst
    let toCsv (OutputText t) =
        let toCsvHeader (xss:  OutputSentence list) =
            let findLongestSentence ss =
                let folder acc (OutputSentence s) = if acc <= List.length s then List.length s else acc 
                List.fold folder 0 ss
            let i = findLongestSentence xss//List.maxBy (fun (Sentence s) -> List.length s) xss |> ( fun os -> match os with | Sentence s -> List.length s
            let headerCells = 
                [1..i]
                |> List.map ((sprintf "Word %i") >> Cell) 
            DummyCell::headerCells |> Row          

        let toCsvBody (xss:  OutputSentence list) =
            let toCsvRow index (OutputSentence ws) =
                let toCvsCell (Word w) = Cell w
                let cells = ws |> List.map toCvsCell
                let rh = sprintf "Sentence %i" (index + 1) |> Cell
                rh::cells |> Row
            xss |> List.mapi toCsvRow

        let h = t |> toCsvHeader
        let rows = t |> toCsvBody
        h::rows |> Csv        

open TextAst
let text = 
    [["A"; "B"];["C"; "D"];["Z"; "F"]; ["y"; "G"]; ["X"; "h"]]
    |> List.map 
        (fun sen -> 
            sen
            |> List.map Word
            |> InputSentence)
    |> InputText 


text |> printfn "%A"    
sortCaseInsensetive text |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> XmlAst.toCompactString |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXDocument |> Result.bind (XDocAst.toString) |> printfn "%A"
