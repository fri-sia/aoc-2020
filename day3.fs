namespace AOC

open System.IO

module Day3 =
    let trees_encountered dx dy (map: list<string>) =
        let bitscalars = seq {for i in 0 .. dy .. (map.Length - 1) ->
                 match (map.[i].[((i/dy)*dx) % map.[0].Length]) with
                   | '.' -> 0
                   | '#' -> 1
                   | _   -> failwith "invalid input" }
        Seq.fold (+) 0 bitscalars

    [<EntryPoint>]
    let main args =
        printfn "Part 1:"
        let lines = List.ofSeq(System.IO.File.ReadLines("./input/day3.input"))
        let trees = trees_encountered 3 1 lines
        printfn "Sum: %i\n" trees

        printfn "Part 2"
        let slopes =
            [(1, 1);
             (3, 1);
             (5, 1);
             (7, 1);
             (1, 2)]
        let all_encountered =
            seq {for pair in slopes ->
                 let (dx, dy) = pair
                 bigint(trees_encountered dx dy lines)}

        printfn "Slopes: %A" all_encountered
        printfn "Last slopes encountered: %A" (List.ofSeq(all_encountered).[4])
        let product : bigint = Seq.fold (*) (bigint(1)) all_encountered
        printfn "Product: %A" product

        0
