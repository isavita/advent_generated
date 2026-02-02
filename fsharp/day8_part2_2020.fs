
open System
open System.IO

let parseInstruction (instruction: string) =
    let parts = instruction.Split(' ')
    (parts.[0], int parts.[1])

let executeBootCode (instructions: string[]) =
    let rec loop accumulator visited currentInstruction =
        if currentInstruction >= instructions.Length then
            (accumulator, true)
        elif Array.contains currentInstruction visited then
            (accumulator, false)
        else
            let visited' = Array.append visited [|currentInstruction|]
            let (op, arg) = parseInstruction instructions.[currentInstruction]
            match op with
            | "acc" -> loop (accumulator + arg) visited' (currentInstruction + 1)
            | "jmp" -> loop accumulator visited' (currentInstruction + arg)
            | _ -> loop accumulator visited' (currentInstruction + 1)

    loop 0 [||] 0

let main () =
    let instructions = File.ReadAllLines "input.txt"
    let mutable found = false
    for i in 0 .. instructions.Length - 1 do
        if not found then
            let (op, arg) = parseInstruction instructions.[i]
            if op = "acc" then () else
            let modifiedInstructions =
                Array.init instructions.Length (fun j ->
                    if j = i then
                        if op = "jmp" then $"nop {arg}" else $"jmp {arg}"
                    else instructions.[j])
            let (accumulator, terminated) = executeBootCode modifiedInstructions
            if terminated then
                printfn "%d" accumulator
                found <- true

main ()
