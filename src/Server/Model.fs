#if !INTERACTIVE
module Model
#endif

#if INTERACTIVE
#r "System.Core.dll"
#r "System.Xml.Linq" 
#endif

module TextAst =
    type Word = Word of string

    module Input =
        type Sentence = Sentence of Word list
        type InputText = Text of Sentence list

    module Output = 
        type OutputSentence = Sentence of Word list
        type OutputText = Text of OutputSentence list
    
    let sortCaseInsensetive (Input.Text xs) =
        let sentenceFolder acc (Input.Sentence sentence) =
            let toLower (x:string) = x.ToLower() 
            let s = sentence 
                    |> List.sortBy ( fun (Word w) -> toLower w)
                    |> Output.Sentence
            acc @ [s]
        xs
        |> List.fold sentenceFolder [] 
        |> Output.Text
open TextAst.Output

module XmlAst =
    type XmlWord =  XmlWord of string 
    type XmlSentence = XmlSentence of XmlWord list 
    type XmlTextBody = XmlTextBody of XmlSentence list 
    type XmlHeader = XmlHeader of string
    type XmlText = XmlText of XmlHeader * XmlTextBody
    
    let private tag tag s  = sprintf "<%s>%s</%s>" tag s tag

    let private foldWords ws =
        let folder acc x = acc + x
        let wordTag = tag "word"
        let wordToString (XmlWord w) = wordTag w 
        ws
        |> List.map wordToString
        |> List.fold folder ""

    let private foldSentences sentences =
        let folder acc x = acc + x
        let sentenceTag = tag "sentence"
        let sentenceToString (XmlSentence ws) = sentenceTag (foldWords ws)
        let words = 
            sentences
            |> List.map sentenceToString
            |> List.fold folder ""
        words

    let private bodyToString (XmlTextBody sentences) =
        let textTag = tag "text"
        textTag (sentences |> foldSentences )

    let toString (XmlText (header, body)): string =
        let (XmlHeader strHeader) = header
        strHeader + (bodyToString body)

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

    type HeaderCell = HeaderCell of string

    type DummyCell = DummyCell
    type ColumnHeader = Header of DummyCell*HeaderCell list
    type Cell = Cell of string
    type RowHeader = RowHeader of string
    type Row = Row of RowHeader * Cell list
    type Csv = Csv of ColumnHeader * Row list
    let toString csv = sprintf "%A" csv
open CsvAst

module Mappers =
    open TextAst

    open XmlAst
    let toXml t =
        let toXmlHeader () = XmlHeader """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""   
        let toXmlWord (Word w) = XmlWord w
        let toXmlSentence (Output.Sentence s) = s |> List.map toXmlWord |> XmlSentence    
        let toXmlTextBody (Output.Text ss) =  ss |> List.map toXmlSentence |> XmlTextBody
        let xmlTextBody = toXmlTextBody t
        let header = toXmlHeader()
        XmlText (header, xmlTextBody)
    
    open XDocAst
    let toXDocument (Output.Text t) =
        let toXWord (Word w) = XElement "word" [w]
        let toXWords ws = ws |> List.map toXWord
        let toXSentence (Output.Sentence s) = XElement "sentence" (s |> toXWords )
        let xText = XElement "text" (t |> List.map toXSentence )
        try
            let xDoc = XDocument (XDeclaration "1.0" "UTF-8" "yes") [xText]
            Result.Ok xDoc
        with ex -> Result.Error (ex.ToString())
    
    let toCsv (Output.Text t) =
        let toHeader (xss:  OutputSentence list) =
            let findLongestSentence ss =
                let folder acc (Output.Sentence s) = if acc <= List.length s then List.length s else acc 
                List.fold folder 0 ss
            let i = findLongestSentence xss//List.maxBy (fun (Sentence s) -> List.length s) xss |> ( fun os -> match os with | Sentence s -> List.length s
            let headerCells = 
                [1..i]
                |> List.map ((sprintf "Word %i") >> HeaderCell.HeaderCell) 
            (DummyCell, headerCells) |> Header          
        let h = t |> toHeader
        let toCsvRows (xss:  OutputSentence list) =
            let toRow index (Output.Sentence ws) =
                let cells = ws |> List.map (fun (Word w) -> Cell w)
                let rh = sprintf "Sentence %i" (index + 1) |> RowHeader
                (rh, cells) |> Row
            xss |> List.mapi toRow

        let rows = t |> toCsvRows
        (h, rows) |> Csv        

open TextAst
open TextAst.Input
let text = 
    [["A"; "B"];["C"; "D"];["Z"; "F"]; ["y"; "G"]; ["X"; "h"]]
    |> List.map 
        (fun sen -> 
            sen
            |> List.map Word
            |> Sentence)
    |> Text 


text |> printfn "%A"    
sortCaseInsensetive text |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> XmlAst.toString |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXDocument |> Result.bind (XDocAst.toString) |> printfn "%A"
