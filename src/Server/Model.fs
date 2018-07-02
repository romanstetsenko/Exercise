#if !INTERACTIVE
module Model
#endif

#if INTERACTIVE
#r "System.Core.dll"
#r "System.Xml.Linq" 
#endif

module TextCat =
    type Word = Word of string
    type Sentence = Sentence of Word list
    type SortedSentence = SortedSentence of Word list
    type InputText = InputText of Sentence list
    type SortedText = SortedText of SortedSentence list

module XmlCat =
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

module XDocCat =
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

module Transformers =
    open TextCat

    open XmlCat
    let toXml st =
        let toXmlHeader () = XmlHeader """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>"""   
        let toXmlWord (Word w) = XmlWord w
        let toXmlSentence (SortedSentence s) = s |> List.map toXmlWord |> XmlSentence    
        let toXmlTextBody (SortedText ss) =  ss |> List.map toXmlSentence |> XmlTextBody
        let xmlTextBody = toXmlTextBody st
        let header = toXmlHeader()
        XmlText (header, xmlTextBody)
    
    open XDocCat
    let toXDocument (SortedText st) =
        let toXWord (Word w) = XElement "word" [w]
        let toXWords ws = ws |> List.map toXWord
        let toXSentence (SortedSentence s) = XElement "sentence" (s |> toXWords )
        let xText = XElement "text" (st |> List.map toXSentence )
        try
            let xDoc = XDocument (XDeclaration "1.0" "UTF-8" "yes") [xText]
            Result.Ok xDoc
        with ex -> Result.Error (ex.ToString())
    
    let sortCaseInsensetive (InputText xs) =
        let sentenceFolder acc (Sentence sentence) =
            let toLower (x:string) = x.ToLower() 
            let s = sentence 
                    |> List.sortBy ( fun (Word w) -> toLower w)
                    |> SortedSentence
            acc @ [s]
        xs
        |> List.fold sentenceFolder [] 
        |> SortedText           

open TextCat



let transformSentences sentences =
    sentences
    |> List.map 

let text = 
    [["A"; "B"];["C"; "D"];["Z"; "F"]; ["y"; "G"]; ["X"; "h"]]
    |> List.map 
        (fun sen -> 
            sen
            |> List.map Word
            |> Sentence)
    |> InputText 


text |> printfn "%A"    
Transformers.sortCaseInsensetive text |> printfn "%A"
Transformers.sortCaseInsensetive text |> Transformers.toXml |> printfn "%A"
Transformers.sortCaseInsensetive text |> Transformers.toXml |> XmlCat.toString |> printfn "%A"
Transformers.sortCaseInsensetive text |> Transformers.toXDocument |> Result.bind (XDocCat.toString) |> printfn "%A"
