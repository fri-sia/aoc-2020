namespace AOC

open System.IO
open System.Linq
open System.Text.RegularExpressions

module Day4 =

    let requiredFields =
        [ "byr:"
        ; "iyr:"
        ; "eyr:"
        ; "hgt:"
        ; "hcl:"
        ; "ecl:"
        ; "pid:" ]

    let validRecord (record: string): bool =
        let fieldsMet =
            seq { for req in requiredFields ->
                  record.Contains req }
        Seq.fold (&&) true fieldsMet

    let validHeight (w: string): bool =
        let cmPattern = "\d+cm"
        let inchPattern = "\d+in"
        let mCm = Regex.Match(w, cmPattern)
        let mInch = Regex.Match(w, inchPattern)
        if mCm.Success then
            let v = int(mCm.Value.Remove(mCm.Value.Length - 2))
            in 150 <= v && v <= 193
        else if mInch.Success then
            let v = int(mInch.Value.Remove(mInch.Value.Length - 2))
            in 59 <= v && v <= 76
        else false

    let validField (field: string): bool =
        match field.Split(":") with
            | [| "byr"; w |] ->
                if Regex.Match(w, "\d{4}").Success
                then let v = int(w)
                     in 1920 <= v && v <= 2002
                else false
            | [| "iyr"; w |] ->
                if Regex.Match(w, "\d{4}").Success
                then let v = int(w)
                     in 2010 <= v && v <= 2020
                else false
            | [| "eyr"; w |] ->
                if Regex.Match(w, "\d{4}").Success
                then let v = int(w)
                     in 2020 <= v && v <= 2030
                else false
            | [| "ecl"; w |] -> Regex.Match(w, "^(amb|blu|brn|gry|grn|hzl|oth)$").Success
            | [| "hgt"; w |] -> validHeight(w)
            | [| "hcl"; w |] -> Regex.Match(w, "^#[0-9a-f]{6}$").Success
            | [| "pid"; w |] -> Regex.Match(w, "^\d{9}$").Success
            | _ -> true

    let validRecord2 (record: string): bool =
        let fields = Regex.Split(record, "\s+|\n")
        let fieldsMet =
            seq { for f in fields ->
                  validField(f) }
        Seq.fold (&&) true fieldsMet
    
    [<EntryPoint>]
    let main args =
        let input = System.IO.File.ReadAllText("./input/day4.input")
        let records = input.Split("\n\n")
        printfn "records: %i" records.Length
        let validRecords = Seq.filter validRecord records
        printfn "valid records: %i\n" (validRecords.Count())

        let validRecords2 = Seq.filter validRecord2 validRecords
        printfn "valid records 2: %i\n" (validRecords2.Count())
        0
