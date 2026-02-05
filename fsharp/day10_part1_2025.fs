open System
open System.IO

[<EntryPoint>]
let main argv =
    let MAXR = 64
    let MAXC = 64

    let lines = File.ReadAllLines("input.txt")
    let mutable totalPresses = 0

    let Bits (x:int) =
        let mutable c = 0
        let mutable v = x
        while v <> 0 do
            v <- v &&& (v - 1)
            c <- c + 1
        c

    for line in lines do
        let start = line.IndexOf('[')
        if start <> -1 then
            let endp = line.IndexOf(']', start)
            if endp <> -1 then
                let config = line.Substring(start + 1, endp - start - 1)
                let R = config.Length
                let target = Array.zeroCreate R
                for i = 0 to R - 1 do
                    target.[i] <- if config.[i] = '#' then 1 else 0

                // Parse buttons
                let buttons = Array.init MAXC (fun _ -> Array.zeroCreate MAXR)
                let btnLen = Array.zeroCreate MAXC
                let mutable C = 0
                let mutable p = endp + 1
                let mutable parsing = true
                while parsing do
                    let pos = line.IndexOf('(', p)
                    if pos = -1 then
                        parsing <- false
                    else
                        let e2 = line.IndexOf(')', pos)
                        if e2 = -1 then
                            parsing <- false
                        else
                            let inner = line.Substring(pos + 1, e2 - pos - 1)
                            let parts = inner.Split(',')
                            btnLen.[C] <- 0
                            for tok in parts do
                                let t = int (tok.Trim())
                                buttons.[C].[btnLen.[C]] <- t
                                btnLen.[C] <- btnLen.[C] + 1
                            C <- C + 1
                            p <- e2 + 1

                // Build matrix R x (C+1)
                let matrix = Array2D.zeroCreate R (C + 1)
                for r = 0 to R - 1 do
                    if C > 0 then
                        for c = 0 to C - 1 do
                            matrix.[r, c] <- 0
                            let mutable found = false
                            if btnLen.[c] > 0 then
                                for k = 0 to btnLen.[c] - 1 do
                                    if (not found) && (buttons.[c].[k] = r) then
                                        matrix.[r, c] <- 1
                                        found <- true
                    matrix.[r, C] <- target.[r]

                // Gaussian Elimination Min Weight
                if C > 0 || R = 0 then
                    let mutable colIsPivot = Array.zeroCreate C
                    let mutable pivotRow = 0

                    for c = 0 to C - 1 do
                        if pivotRow < R then
                            let mutable sel = -1
                            let mutable r = pivotRow
                            while r < R && sel = -1 do
                                if matrix.[r, c] = 1 then sel <- r
                                r <- r + 1
                            if sel <> -1 then
                                // swap rows pivotRow and sel
                                for k = 0 to C do
                                    let t = matrix.[pivotRow, k]
                                    matrix.[pivotRow, k] <- matrix.[sel, k]
                                    matrix.[sel, k] <- t
                                // eliminate other rows
                                for rr = 0 to R - 1 do
                                    if rr <> pivotRow && matrix.[rr, c] = 1 then
                                        for k = c to C do
                                            matrix.[rr, k] <- matrix.[rr, k] ^^^ matrix.[pivotRow, k]
                                colIsPivot.[c] <- true
                                pivotRow <- pivotRow + 1

                    let mutable possible = true
                    for r = pivotRow to R - 1 do
                        if matrix.[r, C] = 1 then possible <- false

                    if possible then
                        let freeVars = Array.zeroCreate C
                        let mutable nFree = 0
                        for c = 0 to C - 1 do
                            if not colIsPivot.[c] then
                                freeVars.[nFree] <- c
                                nFree <- nFree + 1

                        let mutable minWeight = Int32.MaxValue
                        let limit = 1 <<< nFree
                        for i = 0 to limit - 1 do
                            let x = Array.zeroCreate C
                            let mutable cw = Bits i
                            for j = 0 to nFree - 1 do
                                if (((i >>> j) &&& 1) = 1) then
                                    x.[freeVars.[j]] <- 1
                            let mutable currPivotRow = 0
                            for c = 0 to C - 1 do
                                if colIsPivot.[c] then
                                    let mutable valv = matrix.[currPivotRow, C]
                                    if c < C - 1 then
                                        for k = c + 1 to C - 1 do
                                            if matrix.[currPivotRow, k] = 1 then
                                                valv <- valv ^^^ x.[k]
                                    x.[c] <- valv
                                    if valv = 1 then cw <- cw + 1
                                    currPivotRow <- currPivotRow + 1
                            if cw < minWeight then minWeight <- cw
                        totalPresses <- totalPresses + minWeight

    printfn "%d" totalPresses
    0