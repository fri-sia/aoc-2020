namespace AOC

open System.IO
open System.Linq
open System.Diagnostics

module Day11 =

    type Cell = OccupiedSeat | EmptySeat | Floor

    let readGrid =
        let parseChar c = match c with
                            | '.' -> Floor
                            | '#' -> OccupiedSeat
                            | 'L' -> EmptySeat
                            | _ -> failwith "Invalid input"
        let parseLine = Seq.map parseChar >> Array.ofSeq
        System.IO.File.ReadLines("./input/day11.input")
        |> Seq.map parseLine
        |> Array.ofSeq

    let showCell cell = match cell with
                         | Floor -> '.'
                         | OccupiedSeat -> '#'
                         | EmptySeat -> 'L'

    let showGrid (grid: Cell[][]) =
        for line in grid do
            let s = Seq.map showCell line
            for c in s do
                printf "%c" c
            printfn ""

    let occupiedNeighbors (grid: Cell[][]) y x =
        let h = grid.Length
        let w = grid.[0].Length
        let dxs = [-1; 0; 1]
        let dys = [-1; 0; 1]
        let dps = seq { for x in dxs do
                        for y in dys ->
                        (y, x)}
        let neighbors =
            dps
            |> Seq.filter (fun t -> not (t = (0,0)))
            |> Seq.map (fun (dy, dx) -> (y + dy, x + dx))
            |> Seq.filter (fun (y, x) -> y < h && x < w && y >= 0 && x >= 0)
            |> Seq.map (fun (y, x) -> grid.[y].[x])
        let occupiedNeighbors =
            neighbors
            |> Seq.filter (fun x -> x = OccupiedSeat)
            |> Seq.fold (fun acc _ -> acc + 1) 0
        occupiedNeighbors

    let extendVector (y,x) =
        Seq.initInfinite (fun i ->
                          let fact = i + 1
                          in (y*fact, x*fact))

    let visibleNeighbors (grid: Cell[][]) y x =
        let h = grid.Length
        let w = grid.[0].Length
        let dxs = [-1; 0; 1]
        let dys = [-1; 0; 1]
        let dps = seq { for x in dxs do
                        for y in dys do
                        if not ((y,x) = (0,0))
                        then yield (y, x)}
        let inBounds grid (y, x) = x >= 0 && y >= 0 && x < w && y < h
        let vectorMeetsNeighbor vec =
            let s =
                seq {for (y, x) in vec do
                     if not (inBounds grid (y, x))
                     then yield None
                     else
                         match grid.[y].[x] with
                             | Floor -> ()
                             | _ -> yield Some((y,x))
                             }
            Seq.head s
        let s = seq {for vec in dps do
                     let v = Seq.map (fun (dy, dx) -> (y + dy, x + dx)) (extendVector vec)
                     let n = vectorMeetsNeighbor v
                     match n with
                         | Some(x) -> yield x
                         | _ -> ()
                     }
        s |> Array.ofSeq

    let makeNeighborGrid (grid: Cell[][]) =
        let h = grid.Length - 1
        let w = grid.[0].Length - 1
        let line = seq {for x in 0..w -> x}
        let ngrid =
            seq { for y in 0..h ->
                  Seq.map (visibleNeighbors grid y) line
                  |> Array.ofSeq}
        ngrid |> Array.ofSeq


    let updateCell (grid: Cell[][]) (neighborGrid: (int * int)[][][]) y x =
        let cellType = grid.[y].[x]
        let neighbors = neighborGrid.[y].[x]
        let neighborCount =
            neighbors
            |> Seq.filter (fun (y,x) -> grid.[y].[x] = OccupiedSeat)
            |> (fun x -> x.Count())
        match (cellType, neighborCount) with
            | (EmptySeat, 0) -> OccupiedSeat
            | (OccupiedSeat, n) when n > 4 -> EmptySeat
            | (cell, _) -> cell

    let updateGrid (grid: Cell[][]) (ngrid: (int * int)[][][]) =
        let h = grid.Length - 1
        let w = grid.[0].Length - 1
        let line = seq {for x in 0..w -> x}
        let newGrid =
            seq { for y in 0..h ->
                  Seq.map (updateCell grid ngrid y) line
                  |> Array.ofSeq }
        newGrid |> Array.ofSeq

    let countSeats grid =
        let countCell cell =
            if cell = OccupiedSeat
            then 1 else 0
        let nmap = Seq.map (Seq.map countCell) grid
        Seq.fold (fun acc line -> Seq.fold (+) acc line) 0 nmap

    [<EntryPoint>]
    let main args =
        let timer = new Stopwatch()
        timer.Start()
        let grid : Cell[][] = readGrid
        let ngrid : (int * int)[][][] = makeNeighborGrid grid
        let grids =
            grid
            |> Seq.unfold (fun x -> Some(x, updateGrid x ngrid))

        let seats = grids |> Seq.map countSeats
        let zipped = Seq.zip seats (Seq.skip 1 seats)
        let (stabilisedSeatNumber,_) =
            zipped
            |> Seq.skipWhile (fun (a,b) -> not (a = b))
            |> Seq.head
        printfn "Stabilised seat number: %i" stabilisedSeatNumber
        timer.Stop()
        printfn "Time: %i" timer.ElapsedMilliseconds
        0
