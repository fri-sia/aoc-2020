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

    let invalidValues (rules: seq<seq<int*int>>) : seq<int> -> seq<int> =
        let innerPred n = Seq.exists (fun (lr, hr) -> (lr <= n && n <= hr))
        Seq.filter (fun n ->
                    not (Seq.exists (innerPred n) rules))

    let validTicket rules = Seq.filter (invalidValues rules >> Seq.isEmpty)

    [<EntryPoint>]
    let main args =
        let input = System.IO.File.ReadAllText("./input/day16.input")
        let [| rulesText; myTicketText; otherTicketsText |] = input.Split("\n\n")
        let rules = parseRules (rulesText.Trim()) |> Seq.map snd
        let tickets = parseOtherTickets (otherTicketsText.Trim())
        let invalidValues =
            tickets
            |> Seq.map (invalidValues (Seq.map Seq.ofList rules))
            |> Seq.concat
        let scanningRate = Seq.fold (+) 0 invalidValues
        printfn "scanning rate: %A" scanningRate
        0
