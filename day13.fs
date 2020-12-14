namespace AOC

open System.IO

module Day13 =
    let parseFactors (factors: string) =
        factors.Split(",")
        |> Seq.zip (seq { 0..1000 })
        |> Seq.filter (not << (fun (n, s) -> s = "x"))
        |> Seq.map (fun (idx, s) -> (idx, bigint(int(s))))

    let readInput =
        let lines =  System.IO.File.ReadLines("./input/short_day13.input")
                  |> List.ofSeq
        match lines with
            | [ start; factors ] ->
                (int(start), parseFactors(factors))
            | _ -> failwith "invalid input"

    let earliestDeparture factors start =
        factors
        |> Seq.map (fun a -> (a - (start % a), a))
        |> Seq.sortBy (fun (a,b) -> a)
        |> Seq.head

    [<EntryPoint>]
    let main args =
        let (_, factors) = readInput
        printfn "%A" factors
        0
