namespace AOC

open System.IO

type InstructionType = Acc | Jmp | Nop

type Instruction = Instruction of InstructionType * int

type MachineState =
    { Accumulator: int
      VisitedInstructions: Set<int>
      StackPointer: int
      ChangedInstruction: bool }

module Day8 =
    let parseInstruction (line: string) =
        let [| instr; arg |] = line.Split(" ")
        let instrType = match instr with
                          | "nop" -> Nop
                          | "acc" -> Acc
                          | "jmp" -> Jmp
                          | _ -> failwith "invalid input"
        Instruction(instrType, int(arg))

    let runInstruction s instr =
        match instr with
            | Instruction(Nop, _) -> { s with StackPointer = s.StackPointer  + 1}
            | Instruction(Acc, n) -> { s with Accumulator = s.Accumulator + n;
                                              StackPointer = s.StackPointer + 1 }
            | Instruction(Jmp, n) -> { s with StackPointer = s.StackPointer + n}

    let flipInstruction instr =
        match instr with
            | Instruction(Nop, n) -> Instruction(Jmp, n)
            | Instruction(Jmp, n) -> Instruction(Nop, n)
            | x -> x

    let runModifyInstruction s instr =
        let firstState = runInstruction s instr
        if s.ChangedInstruction
        then [| firstState |]
        else
            let secondState = runInstruction { s with ChangedInstruction = true } (flipInstruction instr)
            [| firstState; secondState |]

    let rec runProgram (prog: array<Instruction>) s =
        match (Set.contains s.StackPointer s.VisitedInstructions) with
            | true -> s
            | false ->
                let instr = prog.[s.StackPointer]
                let ss = { s with VisitedInstructions = s.VisitedInstructions.Add(s.StackPointer) }
                runProgram prog (runInstruction ss instr)

    let rec runAndModifyProgram (prog: array<Instruction>) s =
        match (Set.contains s.StackPointer s.VisitedInstructions) with
            | true -> None
            | false ->
                if s.StackPointer >= prog.Length
                then Some(s)
                else
                    let instr = prog.[s.StackPointer]
                    let ss = { s with VisitedInstructions = s.VisitedInstructions.Add(s.StackPointer) }
                    let newStates = runModifyInstruction ss instr
                    newStates
                    |> Seq.map (runAndModifyProgram prog)
                    |> Seq.filter (fun x -> x.IsSome)
                    |> Seq.tryPick (fun x -> x)

    [<EntryPoint>]
    let main args =
        let program =
            System.IO.File.ReadLines("./input/day8.input")
            |> Seq.map parseInstruction
            |> Array.ofSeq
        let initialState =
            { Accumulator  = 0; VisitedInstructions = Set.empty;
              StackPointer = 0; ChangedInstruction  = false }
        let s = runProgram program initialState
        let v = s.Accumulator
        printfn "Accumulator: %i" v

        let ss = runAndModifyProgram program initialState
        printfn "State: %A" ss
        0
