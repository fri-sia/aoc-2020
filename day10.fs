namespace AOC

open System.IO
open System.Linq
open System.Diagnostics

module Day10 =
    let readInput =
        System.IO.File.ReadLines("./input/short_day10.input")
        |> Seq.map int
        |> Seq.append (Seq.singleton 0)
        |> Seq.sort
        |> Array.ofSeq

    let makeChain jolts =
        let zippedJolts = Seq.zip (Seq.skip 1 jolts) jolts
        let diffs = Seq.map (fun (a, b) -> a - b) zippedJolts
        let chain = Seq.takeWhile (fun x -> x < 4) diffs
        chain

    let splitBy f input =
        let i = ref 0
        let tupled =
            seq {for x in input do
                 if f x then incr i
                 else yield (i, x)}
        tupled
        |> Seq.groupBy fst
        |> Seq.map (fun (_, b) -> Seq.map snd b)

    let part1 chain =
        let oneDiffs =
            chain
            |> Seq.filter (fun x -> x = 1)
            |> (fun s -> s.Count())
        let threeDiffs =
            chain
            |> Seq.filter (fun x -> x = 3)
            |> (fun s -> s.Count())
            |> (fun n -> n + 1)

        printfn "product: %i" (oneDiffs * threeDiffs)

    let rec countPathsInGroup group =
        let sums =
            group
            |> Seq.scan (+) 0
            |> Seq.skip 1
            |> Seq.takeWhile (fun x -> x < 4)
        let n = sums.Count()
        if n < 2
        then 1
        else
            let paths =
                seq { for x in 1..n ->
                      let s = Seq.skip x group
                      countPathsInGroup s }
            Seq.fold (+) 0 paths

    let rec part2 chain =
        let groups = splitBy (fun x -> x = 3) chain
        Seq.map countPathsInGroup groups

    let reach (a,b,c) =
        if a + b + c < 4
        then 3
        else
            if b + c < 4
            then 2
            else 1

    // testing a different method
    let part2hyper chain =
        let tri = ref (3,3,3)
        let waysToReach =
            seq {for elem in chain ->
                 let (a,b,c) = !tri
                 let v = reach (a,b,c)
                 tri := (b,c,elem)
                 v }
        for v in waysToReach do
            printfn "%A" v

        let s =
            waysToReach
            |> Seq.fold (fun acc n -> acc * int64(n)) 1L
        s


    [<EntryPoint>]
    let main args =
        let jolts = readInput
        let chain = makeChain jolts
        part1 chain

        let timer = new Stopwatch()
        timer.Start()
        let combinations = part2 chain
        let n = (Seq.fold (fun acc n -> acc * int64(n)) 1L combinations)
        timer.Stop()
        printfn "n: %i" n
        printfn "time: %i" (timer.ElapsedMilliseconds)
        let nn = part2hyper chain
        printfn "n: %i" nn
        0
