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
    type ParsedText = | Text of Sentence list

    type SortedSentence = | SortedSentence of Word list
    type SortedText = | SortedText of SortedSentence list
    
    let sortCaseInsensetive (Text xs) =
        let sentenceFolder acc (Sentence sentence) =
            let toLower (x:string) = x.ToLower() 
            let s = sentence 
                    |> List.sortBy ( fun (Word w) -> toLower w)
                    |> SortedSentence
            acc @ [s]
        xs
        |> List.fold sentenceFolder [] 
        |> SortedText

module XmlAst =
    type XmlWord = | XmlWord of string 
    type XmlSentence = | XmlSentence of XmlWord list 
    type XmlText = | XmlText of XmlSentence list 
    type XmlHeader = | XmlHeader of string
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
        let renderRow = fun (Row r) -> r |> List.map renderCell |> String.concat ", "        
        rows
        |> List.map renderRow
        |> String.concat "\n" 

open CsvAst
open XmlAst

type OutputFormat = | Csv | Xml

module Mappers =
    open TextAst

    let toXml (SortedText t) =
        let toXmlHeader () = XmlHeader """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""   
        let toXmlWord (Word w) = XmlWord w
        let toXmlSentence (SortedSentence s) = s |> List.map toXmlWord |> XmlSentence    
        let toXmlText ss =  ss |> List.map toXmlSentence |> XmlText
        let text = toXmlText t
        let header = toXmlHeader ()
        XmlAst.Xml (header, text)
    
    open XDocAst
    let toXDocument (SortedText t) =
        let toXWord (Word w) = XElement "word" [w]
        let toXWords ws = ws |> List.map toXWord
        let toXSentence (SortedSentence s) = XElement "sentence" (s |> toXWords )
        let xText = XElement "text" (t |> List.map toXSentence )
        try
            let xDoc = XDocument (XDeclaration "1.0" "UTF-8" "yes") [xText]
            Result.Ok xDoc
        with ex -> Result.Error (ex.ToString())
    
    type Mapper<'Out> (mapWord, mapSentence, mapText) =
        member __.Run (SortedText sentences): 'Out =
            sentences
            |> List.map 
                (fun (SortedSentence words) -> 
                    words
                    |> List.map mapWord
                    |> mapSentence            
                )
            |> mapText        

    open CsvAst
    let toCsv (SortedText t) =
        let toCsvHeader (xss:  SortedSentence list) =
            let findLongestSentence ss =
                let folder acc (SortedSentence s) = if acc <= List.length s then List.length s else acc 
                List.fold folder 0 ss
            let i = findLongestSentence xss//List.maxBy (fun (Sentence s) -> List.length s) xss |> ( fun os -> match os with | Sentence s -> List.length s
            let headerCells = 
                [1..i]
                |> List.map ((sprintf "Word %i") >> Cell) 
            DummyCell::headerCells |> Row          

        let toCsvRows (xss:  SortedSentence list) =
            let toCsvRow index (SortedSentence ws) =
                let toCvsCell (Word w) = Cell w
                let cells = ws |> List.map toCvsCell
                let rh = sprintf "Sentence %i" (index + 1) |> Cell
                rh::cells |> Row
            xss |> List.mapi toCsvRow

        let h = t |> toCsvHeader
        let rows = t |> toCsvRows
        h::rows |> Csv

    //let w = 
    //let csvMapper = new Mapper<Csv>( w, s, t)            

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
sortCaseInsensetive text |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXml |> XmlAst.toCompactString |> printfn "%A"
sortCaseInsensetive text |> Mappers.toXDocument |> Result.bind (XDocAst.toString) |> printfn "%A"

open System
type UseCase_v1 = 
    String -> (* plain input text*)
        ParsedText -> (* parsed and structured input text *)
            ParsedText -> (* now with sorted words *)
                string (*rendered in CSV or XML formats*) 
type UseCase_v1_a = 
    String -> (* plain input text*)
        ParsedText -> (* parsed and structured input text *)
            SortedText -> (* a structure which is isomorphic to the Text structure *) 
                string (* rendered in CSV or XML formats *) 

type UseCase_v2 =
    String -> (* plain input text*)
        ParsedText ->  (* parsed and structured input text *)
            ParsedText -> (* now with sorted words *)
                OutputFormat -> (* CSV or XML strutures, which are also isomorphic to the Parsedtext *)
                    string (* the output structure rendered*)

type UseCase_v2_a =
    String -> (* plain input text*)
        ParsedText -> (* parsed and structured input text *)
            SortedText -> (* a structure which is isomorphic to the Text structure *)
                OutputFormat -> (* CSV or XML strutures, which are also isomorphic to the Parsedtext*)
                    string (* the output structure rendered*)
