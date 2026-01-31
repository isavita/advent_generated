
open System
open System.IO

[<EntryPoint>]
let main _ =
    // read all lines and locate the molecule line (first line after a blank line)
    let lines = File.ReadAllLines("input.txt")
    let mutable molecule = ""
    let mutable i = 0
    while i < lines.Length && molecule = "" do
        if String.IsNullOrWhiteSpace(lines.[i]) && i + 1 < lines.Length then
            molecule <- lines.[i + 1].Trim()
        i <- i + 1

    if String.IsNullOrEmpty(molecule) then
        eprintfn "Molecule not found in input."
        1
    else
        let mutable totalElements = 0
        let mutable rn = 0
        let mutable ar = 0
        let mutable y = 0
        let mutable idx = 0
        while idx < molecule.Length do
            totalElements <- totalElements + 1
            // element starts with an uppercase letter
            let start = idx
            idx <- idx + 1
            if idx < molecule.Length && Char.IsLower(molecule.[idx]) then
                // two‑character element
                let elem = molecule.Substring(start, 2)
                match elem with
                | "Rn" -> rn <- rn + 1
                | "Ar" -> ar <- ar + 1
                | "Y"  -> y  <- y  + 1
                | _    -> ()
                idx <- idx + 1
            else
                // one‑character element
                let elem = molecule.Substring(start, 1)
                match elem with
                | "Y" -> y <- y + 1
                | _   -> ()
        let steps = totalElements - rn - ar - 2 * y - 1
        printfn "%d" steps
        0
