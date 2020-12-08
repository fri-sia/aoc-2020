namespace AOC

open System.IO
open System.Linq

module Day6 =
    let readInput =
        let input = System.IO.File.ReadAllText("./input/day6.input").Trim()
        let groups = input.Split("\n\n")
        groups |> Seq.map (fun s -> Seq.map Set.ofSeq (s.Split("\n")))

    let part1 groups =
        groups
        |> Seq.map Set.unionMany
        |> Seq.map (fun x -> x.Count)
        |> Seq.fold (+) 0

    let part2 groups =
        groups
        |> Seq.map Set.intersectMany
        |> Seq.map (fun x -> x.Count)
        |> Seq.fold (+) 0

    [<EntryPoint>]
    let main args =
        let groups = readInput

        let part1 = part1 groups
        printfn "Part 1: %i" part1

        let part2 = part2 groups
        printfn "Part 2: %i" part2

        0
       
