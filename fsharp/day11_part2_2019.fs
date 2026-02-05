
open System
open System.Collections.Generic
open System.IO

let runRobot initialColor program =
    let mem = Dictionary<int64, int64>()
    program |> Array.iteri (fun i v -> mem.[int64 i] <- v)
    let mutable ip, rb = 0L, 0L
    let mutable pos = (0, 0)
    let mutable dir = 0
    let deltas = [| (0, -1); (1, 0); (0, 1); (-1, 0) |]
    let grid = Dictionary<(int * int), int64>()
    if initialColor <> 0L then grid.[pos] <- initialColor

    let getMem addr = if mem.ContainsKey addr then mem.[addr] else 0L
    let setMem addr v = mem.[addr] <- v

    let getParam mode offset =
        let p = getMem (ip + offset)
        match mode with
        | 0L -> getMem p
        | 1L -> p
        | 2L -> getMem (rb + p)
        | _ -> 0L

    let getDest mode offset =
        let p = getMem (ip + offset)
        match mode with
        | 0L -> p
        | 2L -> rb + p
        | _ -> 0L

    let mutable outputMode = 0
    let mutable halted = false
    while not halted do
        let instr = getMem ip
        let opcode = instr % 100L
        let modes = [| (instr / 100L) % 10L; (instr / 1000L) % 10L; (instr / 10000L) % 10L |]
        match opcode with
        | 1L ->
            setMem (getDest modes.[2] 3L) (getParam modes.[0] 1L + getParam modes.[1] 2L)
            ip <- ip + 4L
        | 2L ->
            setMem (getDest modes.[2] 3L) (getParam modes.[0] 1L * getParam modes.[1] 2L)
            ip <- ip + 4L
        | 3L ->
            let color = if grid.ContainsKey pos then grid.[pos] else 0L
            setMem (getDest modes.[0] 1L) color
            ip <- ip + 2L
        | 4L ->
            let out = getParam modes.[0] 1L
            if outputMode = 0 then
                grid.[pos] <- out
                outputMode <- 1
            else
                if out = 0L then dir <- (dir + 3) % 4 else dir <- (dir + 1) % 4
                let (dx, dy) = deltas.[dir]
                pos <- (fst pos + dx, snd pos + dy)
                outputMode <- 0
            ip <- ip + 2L
        | 5L -> if getParam modes.[0] 1L <> 0L then ip <- getParam modes.[1] 2L else ip <- ip + 3L
        | 6L -> if getParam modes.[0] 1L = 0L then ip <- getParam modes.[1] 2L else ip <- ip + 3L
        | 7L ->
            setMem (getDest modes.[2] 3L) (if getParam modes.[0] 1L < getParam modes.[1] 2L then 1L else 0L)
            ip <- ip + 4L
        | 8L ->
            setMem (getDest modes.[2] 3L) (if getParam modes.[0] 1L = getParam modes.[1] 2L then 1L else 0L)
            ip <- ip + 4L
        | 9L ->
            rb <- rb + getParam modes.[0] 1L
            ip <- ip + 2L
        | 99L -> halted <- true
        | _ -> halted <- true
    grid

[<EntryPoint>]
let main _ =
    let inputStr = File.ReadAllText("input.txt").Trim()
    let program = inputStr.Split([|','; ' '; '\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int64

    let grid1 = runRobot 0L program
    printfn "Part One: %d" grid1.Count

    let grid2 = runRobot 1L program
    let keys = grid2.Keys
    if keys.Count > 0 then
        let minX = keys |> Seq.map fst |> Seq.min
        let maxX = keys |> Seq.map fst |> Seq.max
        let minY = keys |> Seq.map snd |> Seq.min
        let maxY = keys |> Seq.map snd |> Seq.max

        printfn "Part Two:"
        for y in minY .. maxY do
            for x in minX .. maxX do
                let color = if grid2.ContainsKey (x, y) then grid2.[(x, y)] else 0L
                printf "%s" (if color = 1L then "#" else " ")
            printfn ""
    else
        printfn "Part Two: No panels painted."
    0

