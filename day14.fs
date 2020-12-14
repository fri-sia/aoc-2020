namespace AOC

open System.IO

type MaskValue = AlwaysOn | AlwaysOff
type Instruction = ChangeMask of List<int32 * MaskValue> | WriteReg of int64 * int64
type State = State of Map<int64, int64> * List<int32 * MaskValue>

module Day14 =
    let parseMask (m: string) =
        let s = seq {for idx in 0..35 do
                     match m.[idx] with
                     | '1' -> yield (35-idx, AlwaysOn)
                     | '0' -> yield (35-idx, AlwaysOff)
                     | _ -> () }
        List.ofSeq s

    let parseLine (line: string) =
        let [|lhs;rhs|] = line.Split(" = ")
        match lhs with
            | "mask" -> (ChangeMask << parseMask) rhs
            | lhs ->
                let endidx = lhs.Length - 2;
                let startidx = 4;
                let reg = int64(lhs.[startidx..endidx])
                let v = int64(rhs)
                WriteReg(reg, v)

    let readInput =
        System.IO.File.ReadLines("./input/day14.input")
        |> Seq.map parseLine

    let rec maskNumber (mask: List<int32 * MaskValue>) (n: int64) : int64 =
        match mask with
            | [] -> n
            | (idx, AlwaysOn) :: rest ->
                maskNumber rest (n ||| (1L <<< idx))
            | (idx, AlwaysOff) :: rest ->
                maskNumber rest (n &&& ~~~(1L <<< idx))

    let updateState (State(regs, mask)) instr =
        match instr with
            | ChangeMask m -> State (regs, m)
            | WriteReg(reg, v) ->
                let maskedV = maskNumber mask v
                State (Map.add reg maskedV regs, mask)

    [<EntryPoint>]
    let main args =
        printfn "test"
        let prog = readInput
        let state = State (Map.empty, [])
        let (regs, m) = match Seq.fold updateState state prog with
                          | State (a,b) -> (a,b)
        let sum =
            regs
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.fold (+) 0L
        printfn "sum: %i" sum
        0
