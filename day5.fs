namespace AOC

open System.IO

module Day5 =
    let mapChar char =
        match char with
            | 'F' -> '0'
            | 'B' -> '1'
            | 'L' -> '0'
            | 'R' -> '1'
            | _ -> failwith "invalid input"

    let intoBinaryLiteral str =
        "0b" + (String.map mapChar str)

    let parsePass (str: string) =
        let row = int(intoBinaryLiteral(str.[..6]))
        let column = int(intoBinaryLiteral(str.[7..]))
        (row, column, row*8 + column)

    [<EntryPoint>]
    let main args =
        let lines = List.ofSeq(System.IO.File.ReadLines("./input/day5.input"))
        let passes =
            seq { for line in lines ->
                  parsePass line }
        let highestSeatId = Seq.fold (fun acc (_, _, sId) -> max sId acc) 0 passes
        printfn "Highest seat ID: %i" highestSeatId

        passes
        |> Seq.map (fun (_,_, sId) -> sId)
        |> Seq.sortBy (fun x -> x)
        |> (fun x -> Seq.zip x (Seq.skip 1 x)
        |> Seq.filter (fun (s1, s2) -> (s2 - s1) = 2)
        |> Seq.head
        |> fun (s1, s2) -> s1+1
        |> printfn "Your seat: %A"
        
        0
