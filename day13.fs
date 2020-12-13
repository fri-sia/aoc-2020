namespace AOC

open System.IO

module Day13 =
    let parseFactors (factors: string) =
        factors.Split(",")
        |> Seq.filter (not << (fun x -> x = "x"))
        |> Seq.map int

    let readInput =
        let lines =  System.IO.File.ReadLines("./input/day13.input")
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
        let (start, factors) = readInput
        let (minutesWaited, busId) = earliestDeparture factors start
        printfn "%A" (minutesWaited, busId)
        printfn "Product: %i" (minutesWaited * busId)
        0
