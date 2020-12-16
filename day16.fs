namespace AOC

open System.IO

module Day16 =
    let parseRules (s: string) =
        let lines = s.Split("\n")
        [ for line in lines ->
          let [| descriptor; rules |] = line.Split(": ")
          let ranges = [ for range in rules.Split(" or ") ->
                         let [| low; high  |] = range.Split("-")
                         (int(low), int(high))]
          (descriptor, ranges)]

    let parseMyTicket : string -> seq<int> =
        (fun x -> x.Split("\n"))
        >> Seq.skip 1
        >> Seq.head
        >> (fun x -> x.Split(","))
        >> Seq.map int

    let parseOtherTickets: string -> seq<seq<int>> =
        (fun x -> x.Split("\n"))
        >> Seq.skip 1
        >> Seq.map (fun x -> x.Split(","))
        >> Seq.map (Seq.map int)

    let inRange n = Seq.exists (fun (lr, hr) -> (lr <= n && n <= hr))

    let invalidValues (rules: seq<seq<int*int>>) : seq<int> -> seq<int> =
        Seq.filter (fun n ->
                    not (Seq.exists (inRange n) rules))

    let getValidTickets rules = Seq.filter (invalidValues rules >> Seq.isEmpty)

    let reduceOrders orders =
        Seq.pairwise orders
        |> Seq.map (fun ((_, prev), (a, next)) ->
                    let (prevs, nexts) = (Seq.map snd prev, Seq.map snd next)
                    in (a, Seq.find (fun n -> not (Seq.contains n prevs)) nexts))

    [<EntryPoint>]
    let main args =
        let input = System.IO.File.ReadAllText("./input/day16.input")
        let [| rulesText; myTicketText; otherTicketsText |] = input.Split("\n\n")
        let rules = parseRules (rulesText.Trim()) |> Seq.map snd
        let tickets = parseOtherTickets (otherTicketsText.Trim())
        let validTickets = getValidTickets (Seq.map Seq.ofList rules) tickets
        let myTicket = parseMyTicket myTicketText
        let orders =
            seq { for (rangeId, ranges) in Seq.zip [0..100] rules do
                  for i in 0..(Seq.length rules - 1) do
                  let vals = Seq.map (Seq.item i) validTickets
                  if (Seq.forall (fun x -> inRange x ranges) vals)
                  then yield (i, rangeId)}
        let sortedOrders = Seq.groupBy fst orders |> Seq.sortBy (snd >> Seq.length)
        for (i, ranges) in sortedOrders do
            printfn "%A: %A" i ranges
        let reduced = reduceOrders sortedOrders
        for t in reduced do
            printfn "%A" t
        let ns = seq { for (idx, v) in reduced do
                       if v < 6
                       then yield int64(Seq.item idx myTicket) }
        for n in ns do
            printfn "%i" n
        let factor = Seq.fold (*) 1L ns
        printfn "Factor: %A" factor
        0
