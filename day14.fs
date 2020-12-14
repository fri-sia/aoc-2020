namespace AOC

open System.IO

type MaskValue = AlwaysOn | AlwaysOff | Floating
type Instruction = ChangeMask of List<int32 * MaskValue> | WriteReg of int64 * int64
type State = State of Map<int64, int64> * List<int32 * MaskValue>

module Day14 =
    let parseMask (m: string) =
        let s = seq {for idx in 0..35 do
                     match m.[idx] with
                     | '1' -> yield (35-idx, AlwaysOn)
                     | 'X' -> yield (35-idx, Floating)
                     | _ -> ()}
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

    let rec maskNumber (mask: List<int32 * MaskValue>) (n: int64) =
        match mask with
            | [] -> seq { yield n }
            | (idx, AlwaysOn) :: rest ->
                let next = maskNumber rest n
                next
                |> Seq.map (fun n -> n ||| (1L <<< idx))
            | (idx, Floating) :: rest ->
                seq { for x in maskNumber rest n do
                      for y in [ x &&& ~~~(1L <<< idx); x ||| (1L <<< idx)] do
                      yield y }
            | _ :: rest ->
                maskNumber rest n

    let updateState (State(regs, mask)) instr =
        match instr with
            | ChangeMask m -> State (regs, m)
            | WriteReg(reg, v) ->
                let maskedReg = maskNumber mask reg
                let newRegs =
                    Seq.fold (fun m r -> Map.add r v m) regs maskedReg
                State (newRegs, mask)

    [<EntryPoint>]
    let main args =
        let prog = readInput
        let state = State (Map.empty, [])
        for instr in prog do
            printfn "%A" instr
        let (regs, m) = match Seq.fold updateState state prog with
                          | State (a,b) -> (a,b)
        for reg in regs do
            printfn "%A" reg
        let sum =
            regs
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.fold (+) 0L
        printfn "sum: %i" sum
        0
