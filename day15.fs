namespace AOC

module Day15 =

    let rec part1 (m: Map<int, int>) age prevNumber =
        match age with
            | 30000000 -> prevNumber
            | _ ->
                match m.TryFind(prevNumber) with
                    | None -> part1 (m.Add (prevNumber, age)) (age + 1) 0
                    | Some(n) ->
                        let prev = age - n
                        let map = m.Add (prevNumber, age)
                        part1 map (age + 1) prev

    [<EntryPoint>]
    let main args =
        let startMap = Map.ofList [1, 1;
                                   20, 2;
                                   8, 3;
                                   12, 4;
                                   0, 5;]
        let n = part1 startMap 6 14
        printfn "2020th: %i" n
        0

