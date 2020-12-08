namespace AOC

open System.IO

module Day7 =

    let parseParentBagExpr (expr: string) =
        match expr.Split(" ") with
            | [| c1 ; c2 ; _bagsText |] -> c1+c2
            | _ -> failwith "invalid input"

    let parseChildBagExpr expr =
        match expr with
            | [| n ; c1 ; c2 ; _bagsText |] -> (c1+c2, int(n))
            | _ -> ("nocolor", 0)

    let parseLine (line: string) =
        let [| lhs; rhs |] = line.Split(" contain ")
        let childBags = 
            rhs.Split(", ")
            |> Seq.map (fun x -> x.Split(" "))
            |> Seq.map parseChildBagExpr
        let parentBag = parseParentBagExpr lhs
        (parentBag, childBags)

    let buildChildGraph bagSeq : Map<string, Set<string>> =
        let f (graph : Map<string, Set<string>>) (color, childBags) =
            let foldFun (g : Map<string, Set<string>>) (childColor, n) =
                match (Map.tryFind childColor g) with
                    | Some(set) -> Map.add childColor (set.Add(color)) g
                    | None -> Map.add childColor (Set.empty.Add(color)) g
            Seq.fold foldFun graph childBags
        Seq.fold f (Map.empty) bagSeq

    let buildParentGraph bagSeq : Map<string, List<string * int>> =
        let f graph (color, childBags) =
            Map.add color (List.ofSeq childBags) graph
        Seq.fold f (Map.empty) bagSeq


    let rec findBagsContaining color graph =
        match (Map.tryFind color graph) with
            | Some(set) -> Set.union set (Set.unionMany (Seq.map (fun x -> findBagsContaining x graph) set))
            | None -> Set.empty

    let rec findTotalBagsInBag color graph =
        match (Map.tryFind color graph) with
            | Some (lst) ->
                lst
                |> Seq.map (fun (childColor, n) -> n + (findTotalBagsInBag childColor graph) * n)
                |> Seq.fold (+) 0
            | None -> 0
    
    [<EntryPoint>]
    let main args =
        let bagSeq = System.IO.File.ReadLines("./input/day7.input") |> Seq.map parseLine
        let childGraph = buildChildGraph bagSeq
        let allBagsContaining = findBagsContaining "shinygold" childGraph
        printfn "shiny gold containers: %i" (allBagsContaining.Count)

        let parentGraph = buildParentGraph bagSeq
        let bagsInShinyGold = findTotalBagsInBag "shinygold" parentGraph
        printfn "shiny gold containing: %i bags" bagsInShinyGold
        0
