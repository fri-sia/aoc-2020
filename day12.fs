namespace AOC

open System.IO

type Direction = int
type InstructionType = S | N | E | W | F | T
type Instruction = (InstructionType * int)
type Ship = int * int * Direction

module Day12 =
    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else None

    let readInput =
        let parseLine line =
            match line with
                | Prefix "S" res -> (S, int(res))
                | Prefix "N" res -> (N, int(res))
                | Prefix "E" res -> (E, int(res))
                | Prefix "W" res -> (W, int(res))
                | Prefix "F" res -> (F, int(res))
                | Prefix "R" res -> (T, int(res) / 90)
                | Prefix "L" res -> (T, 4-(int(res) / 90))
                | _ -> failwith "invalid input"
        System.IO.File.ReadLines("./input/day12.input")
        |> Seq.map parseLine

    let vecFromDegree dir =
        match dir with
            | 0 -> (1,0)
            | 1 -> (0,1)
            | 2 -> (-1, 0)
            | 3 -> (0, -1)
            | _ -> failwith "invalid input"

    let vecFromDirection dir =
        match dir with
            | E -> (1,0)
            | S -> (0,1)
            | W -> (-1, 0)
            | N -> (0, -1)
            | _ -> failwith "invalid input"

    let moveShip ((x,y,shipDir): Ship) ((instrType, param): Instruction) : Ship =
        match instrType with
            | S | N | E | W ->
                let (dx, dy) = vecFromDirection instrType
                (x + dx * param, y + dy * param, shipDir)
            | T ->
                (x, y, (shipDir + param) % 4)
            | F ->
                let (dx, dy) = vecFromDegree shipDir
                (x + dx * param, y + dy * param, shipDir)

    let rec rotateVec n (x,y) =
        match n with
            | 0 -> (x,y)
            | n -> rotateVec (n-1) (-y, x)

    let moveShipAndWayPoint ((x,y,shipDir), (wx, wy)) ((instrType, param): Instruction) =
        match instrType with
            | S | N | E | W ->
                let (dx, dy) = vecFromDirection instrType
                ((x,y,shipDir), (wx + dx * param, wy + dy * param))
            | T ->
                ( (x,y, shipDir)
                ,  rotateVec param (wx, wy))
            | F ->
                ((x + wx * param, y + wy * param, shipDir), (wx, wy))

    [<EntryPoint>]
    let main args =
        let ship = (0,0,0)
        let (x,y,dir) = Seq.fold moveShip ship readInput
        printfn "Manhattan distance of final position: %i" (abs x + abs y)

        let ((x,y,_), _) = Seq.fold moveShipAndWayPoint (ship, (10, -1)) readInput
        printfn "Part 2 MDistance: %i" (abs x + abs y)
        0
