#if !INTERACTIVE
module Sample
#endif

#if INTERACTIVE
#r "System.Core.dll"
#r @"C:\Users\Roman_Stetsenko\.nuget\packages\fscheck\2.11.0\lib\netstandard1.6\FsCheck.dll"
#r @"C:\Users\Roman_Stetsenko\.nuget\packages\expecto\8.1.1\lib\netstandard2.0\Expecto.dll"
#r @"C:\Users\Roman_Stetsenko\.nuget\packages\expecto.fscheck\8.1.1\lib\netstandard2.0\Expecto.FsCheck.dll"
#endif

open System
open Expecto
open FsCheck

module Gen =
    type Float01 = Float01 of float
    let float01Arb =
        let maxValue = float UInt64.MaxValue
        Arb.convert
            (fun (DoNotSize a) -> float a / maxValue |> Float01)
            (fun (Float01 f) -> f * maxValue + 0.5 |> uint64 |> DoNotSize)
            Arb.from
    type 'a ListOf100 = ListOf100 of 'a list
    //Arb.Default.StringWithoutNullChars
    let listOf100Arb() =
        Gen.listOfLength 100 Arb.generate
        |> Arb.fromGen
        |> Arb.convert ListOf100 (fun (ListOf100 l) -> l)
    type 'a ListOfAtLeast2 = ListOfAtLeast2 of 'a list
    let listOfAtLeast2Arb() =
        Arb.convert
            (fun (h1,h2,t) -> ListOfAtLeast2 (h1::h2::t))
            (function
                | ListOfAtLeast2 (h1::h2::t) -> h1,h2,t
                | e -> failwithf "not possible in listOfAtLeast2Arb: %A" e)
            Arb.from
    let addToConfig config : Expecto.FsCheckConfig =
        {config with arbitrary = typeof<Float01>.DeclaringType::config.arbitrary}

[<AutoOpen>]
module Auto =
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp stdgen name = ftestPropertyWithConfig stdgen config name

module Tests =
    let topicTests =
        testList "topic" [
            testProp "float between 0 and 1" (fun (Gen.Float01 f) ->
                () // test
            )
            testProp "list of 100 things" (fun (Gen.ListOf100 l) ->
                () // test
            )
            testProp "list of at least 2 things" (fun (Gen.ListOfAtLeast2 l) ->
                () // test
            )
            testProp "list of at least 2 things without gen" (fun h1 h2 t ->
                let l = h1::h2::t
                () // test
            )
        ]