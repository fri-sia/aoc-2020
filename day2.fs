namespace AOC

open System.IO

module Day2 =
    let parseLine (line : string) : int * int * char * string =
        match line.Split(" ") with
            | [| range; char; pw; |] ->
                let ranges = range.Split("-")
                let low_range = int(ranges.[0])
                let high_range = int(ranges.[1])
                (low_range, high_range, char.[0], pw)
            | _ -> failwith "invalid input"

    [<EntryPoint>]
    let main args =
        let lines = List.ofSeq(System.IO.File.ReadLines("./input/day2.input"))
        let valid_pws : seq<string> =
            seq {for line in lines do
                 let (lr, hr, ch, pw) = parseLine line
                 let length = (pw |>
                               Seq.filter(fun c -> c = ch) |>
                               Seq.length)
                 if lr <= length && length <= hr
                 then yield pw}

        printfn "valid pws: %i" (Seq.length(valid_pws))

        let valid_pws_2 : seq<string> =
            seq {for line in lines do
                 let (i, k, ch, pw) = parseLine line
                 let first = pw.[i - 1] = ch
                 let second = pw.[k - 1] = ch
                 if (first || second) && not (first && second)
                 then yield pw}

        printfn "valid pws 2: %i" (Seq.length(valid_pws_2))
        0
