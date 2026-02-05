open System
open System.IO
open System.Collections.Generic

let INF = 0x3f3f3f3f
let eps = 1e-9

let parseLine (line: string) : (int[][] * int[] * int[] * int * int) =
    let btnsLists = new List<List<int>>()
    let targetsList = new List<int>()
    let mutable i = 0
    let mutable doneLine = false
    while i < line.Length && not doneLine do
        if line.[i] = '(' then
            i <- i + 1
            let inner = new List<int>()
            while i < line.Length && line.[i] <> ')' do
                let mutable x = 0
                while i < line.Length && Char.IsDigit(line.[i]) do
                    x <- x * 10 + (int line.[i] - int '0')
                    i <- i + 1
                inner.Add(x)
                if i < line.Length && line.[i] = ',' then i <- i + 1
            if i < line.Length && line.[i] = ')' then i <- i + 1
            btnsLists.Add(inner)
        elif line.[i] = '{' then
            i <- i + 1
            while i < line.Length && line.[i] <> '}' do
                let mutable x = 0
                while i < line.Length && Char.IsDigit(line.[i]) do
                    x <- x * 10 + (int line.[i] - int '0')
                    i <- i + 1
                targetsList.Add(x)
                if i < line.Length && line.[i] = ',' then i <- i + 1
            if i < line.Length && line.[i] = '}' then i <- i + 1
            doneLine <- true
        else
            i <- i + 1
    let buttonsArray = btnsLists |> Seq.map (fun lst -> lst.ToArray()) |> Seq.toArray
    let btnSizeArr = buttonsArray |> Array.map (fun a -> a.Length)
    let targetsArr = targetsList.ToArray()
    buttonsArray, btnSizeArr, targetsArr, targetsArr.Length, buttonsArray.Length

let solveFromData (buttons: int[][]) (btnSize: int[]) (targets: int[]) (numCounters: int) (numButtons: int) : int =
    let matrix = Array.init numCounters (fun _ -> Array.create (numButtons + 1) 0.0)
    for j in 0 .. numCounters - 1 do
        matrix.[j].[numButtons] <- float targets.[j]
    for i in 0 .. numButtons - 1 do
        for k in 0 .. btnSize.[i] - 1 do
            let c = buttons.[i].[k]
            if c < numCounters then matrix.[c].[i] <- 1.0
    let pivotCol = Array.create numCounters -1
    let mutable row = 0
    for col in 0 .. numButtons - 1 do
        if row >= numCounters then ()
        else
            let mutable maxRow = row
            for r in row + 1 .. numCounters - 1 do
                if Math.Abs(matrix.[r].[col]) > Math.Abs(matrix.[maxRow].[col]) then maxRow <- r
            if Math.Abs(matrix.[maxRow].[col]) < eps then ()
            else
                if maxRow <> row then
                    for c in 0 .. numButtons do
                        let tmp = matrix.[row].[c]
                        matrix.[row].[c] <- matrix.[maxRow].[c]
                        matrix.[maxRow].[c] <- tmp
                let scale = matrix.[row].[col]
                for c in col .. numButtons do
                    matrix.[row].[c] <- matrix.[row].[c] / scale
                for r in 0 .. numCounters - 1 do
                    if r <> row && Math.Abs(matrix.[r].[col]) > eps then
                        let factor = matrix.[r].[col]
                        for c in col .. numButtons do
                            matrix.[r].[c] <- matrix.[r].[c] - factor * matrix.[row].[c]
                pivotCol.[row] <- col
                row <- row + 1
    let rank = row
    let isPivot = Array.zeroCreate numButtons
    let pivotRows = Array.create numButtons -1
    for r in 0 .. rank - 1 do
        let c = pivotCol.[r]
        if c >= 0 then
            isPivot.[c] <- 1
            pivotRows.[c] <- r
    let freeVarsList = new List<int>()
    for i in 0 .. numButtons - 1 do
        if isPivot.[i] = 0 then freeVarsList.Add(i)
    let freeVarsFinal = freeVarsList.ToArray()
    let numFree = freeVarsFinal.Length

    let maxPresses = Array.create numButtons 0
    for i in 0 .. numButtons - 1 do
        let mutable mp = INF
        for j in 0 .. btnSize.[i] - 1 do
            let c = buttons.[i].[j]
            if c < numCounters && targets.[c] < mp then mp <- targets.[c]
        if mp = INF then mp <- 0
        maxPresses.[i] <- mp

    let freeVarsSorted = freeVarsFinal |> Array.sortBy (fun idx -> maxPresses.[idx])
    let mutable freeValues = Array.zeroCreate freeVarsSorted.Length
    let mutable best = INF

    let rec enumerate (idx: int) (currentSum: int) =
        if currentSum >= best then ()
        elif idx = freeVarsSorted.Length then
            let mutable presses = Array.zeroCreate numButtons
            for i in 0 .. freeVarsSorted.Length - 1 do
                presses.[freeVarsSorted.[i]] <- freeValues.[i]
            let mutable ok = true
            for ri = rank - 1 downto 0 do
                let col = pivotCol.[ri]
                if col >= 0 && ok then
                    let mutable v = matrix.[ri].[numButtons]
                    for c in col + 1 .. numButtons - 1 do
                        v <- v - matrix.[ri].[c] * float presses.[c]
                    let intVal = int (Math.Round v)
                    if Math.Abs(v - float intVal) > 1e-6 then ok <- false
                    if intVal < 0 || intVal > maxPresses.[col] then ok <- false
                    if ok then presses.[col] <- intVal
            if ok then
                let mutable sum = 0
                for i in 0 .. numButtons - 1 do sum <- sum + presses.[i]
                if sum > 0 && sum < best then best <- sum
        else
            let fv = freeVarsSorted.[idx]
            let maxVal = maxPresses.[fv]
            for v in 0 .. maxVal do
                freeValues.[idx] <- v
                enumerate (idx + 1) (currentSum + v)

    enumerate 0 0
    if best = INF then -1 else best

[<EntryPoint>]
let main argv =
    let lines = Directory.GetFiles(".", "input.txt", SearchOption.TopDirectoryOnly)
    // Read actual file content if present
    let path = "input.txt"
    if not (File.Exists(path)) then
        0
    else
        let all = File.ReadAllLines(path)
        let mutable total = 0
        for line in all do
            if not (String.IsNullOrWhiteSpace(line)) then
                let buttonsArr, btnSizeArr, targetsArr, nC, nB = parseLine line
                let res = solveFromData buttonsArr btnSizeArr targetsArr nC nB
                if res > 0 then total <- total + res
        printfn "%d" total
        0