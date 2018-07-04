// #r @"C:\Users\Roman_Stetsenko\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
// open System
// #r @"C:\Users\Roman_Stetsenko\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"
// #load @"Model.fs"

// open Model

// open FParsec
// type UserState = unit // doesn't have to be unit, of course
// type Parser<'t> = Parser<'t, UserState>

// let test p str =
//     match run p str with
//     | Success(result, _, _)   -> printfn "Success: %A" result
//     | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// let text1 = "Mary had a little lamb. Peter called for the wolf, and Aesop came.
// Cinderella likes shoes."
// let text1' = " Mary had a little lamb .
// Peter called for the wolf , and Aesop came .
// Cinderella likes shoes."
// test pText text1
// test pText text1'

// test pAbbreviation "Mr."
// test pAbbreviation "Mr"
// test pSentenceStop "2"
// test pSentenceStop "!?"

// test pSentenceStart "Mr."
// test pSentenceStart "A-a"
// test pCapitalizedWord "A-"

// test pSentence "A b-b j , g ..."
// test pSentence "A-a b-b j , g ."
// test pSentence "Ac-a b-b j , g ."
// test pSentence "A b-b j , g ! "
// test pSentence "A b-b - j , g ..."
// test pSentence "A b-b j , g"
// test pSentence "A b-b,j , g ..."
// test pSentence "A b-b j , g ..."
// test pSentence "A Mr. Bond, come in, please!"
// test pSentence "P.S. Mr.Bond, come in, please!"
// test pSentence "Mr.Bond, come in, please! Next Sentence?"

// test pCapitalizedWord "A"
// test pCapitalizedWord "Ba"
// test pCapitalizedWord "A1 "
// test pCapitalizedWord "A1-1"
// test pCapitalizedWord "A1-"
// test pCapitalizedWord "A1-v"
// test pCapitalizedWord "A1-v   "
// test pCapitalizedWord "b "
// test pCapitalizedWord " A "
// test pCapitalizedWord " A"

// test pRegularWord "1"
// test pRegularWord "a"
// test pRegularWord "j"
// test pRegularWord "a-a"
// test pRegularWord "b-b"
// test pRegularWord "1-1"
// test pRegularWord "1-a"
// test pRegularWord "a-1-"
// test pRegularWord "-a-1"