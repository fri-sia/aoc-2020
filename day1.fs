namespace AOC.Day1

open System.IO

module Day1 =
    let lines = List.ofSeq(System.IO.File.ReadLines("./input/day1.input"))
    let values = lines |> List.map int
    let factor : seq<int> =
        seq {for a in values do
             for b in values do
             if a + b = 2020
             then yield a * b}

    #if INTERACTIVE
    printfn
    printfn "%i\n" (Seq.head(factor))
    #endif

    let factor2 : seq<int> =
        seq {for a in values do
             for b in values do
             for c in values do
             if a + b + c = 2020
             then yield a * b * c}


    #if INTERACTIVE
    printfn
    printfn "%i\n" (Seq.head(factor2))
    #endif
