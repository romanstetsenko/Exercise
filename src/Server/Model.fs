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
