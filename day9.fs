namespace AOC

open System.IO
open System.Linq

module Day9 =
    let readInput =
        System.IO.File.ReadLines("./input/day9.input")
        |> Seq.map int64
        |> Array.ofSeq

    let isValidNumber terms n =
        terms
        |> Seq.map (fun x -> n - x)
        |> Seq.filter (fun x -> not (x + x = n))
        |> Seq.filter (fun x -> Seq.contains x terms)
        |> Seq.tryPick (fun x -> Some(x))
        |> (fun x -> x.IsSome)

    let rec firstInvalidNumber idx (arr: array<int64>) =
        let terms = arr.[(idx - 25)..(idx - 1)]
        let n = arr.[idx]
        if isValidNumber terms n
        then firstInvalidNumber (idx + 1) arr
        else n

    let findRangeSummingTo n (arr: array<int64>) =
        let sequences =
            seq { for i in 0..(arr.Length - 1)  ->
                  Seq.skip i arr }
        seq {for s in sequences do
             let sums = Seq.scan (+) 0L s
             let sn = Seq.map (fun i -> Seq.take i s) (seq { 0..(arr.Length) })
             let zipped = Seq.zip sums sn
             let res =
                 zipped
                 |> Seq.skipWhile (fun (sum, ls) -> sum < n)
                 |> Seq.tryPick (fun x -> Some(x))
             match res with
               | Some(rs) -> yield rs
               | _ -> () }

    [<EntryPoint>]
    let main args =
        let numbers = readInput
        let firstInvalid = firstInvalidNumber 25 numbers
        printfn "First invalid number: %i" firstInvalid

        let (_, ls) =
            findRangeSummingTo firstInvalid numbers
            |> Seq.filter (fun (sum, ls) -> sum = firstInvalid)
            |> Seq.head
        let (h,l) = (Seq.max ls, Seq.min ls)
        printfn "%A" (h + l)
        0
