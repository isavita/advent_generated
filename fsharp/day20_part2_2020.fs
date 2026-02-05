
open System
open System.IO

type Tile = { Id: int; Orientations: char[,] [] }

let rotate (g: char[,]) =
    let n = Array2D.length1 g
    Array2D.init n n (fun r c -> g.[n - 1 - c, r])

let flip (g: char[,]) =
    let n = Array2D.length1 g
    let m = Array2D.length2 g
    Array2D.init n m (fun r c -> g.[r, m - 1 - c])

let getOrientations grid =
    let mutable res = []
    let mutable curr = grid
    for _ in 1..4 do
        res <- curr :: res
        curr <- rotate curr
    curr <- flip grid
    for _ in 1..4 do
        res <- curr :: res
        curr <- rotate curr
    List.toArray res

[<EntryPoint>]
let main _ =
    let content = File.ReadAllText("input.txt")
    let blocks = content.Split([|"\r\n\r\n"; "\n\n"|], StringSplitOptions.RemoveEmptyEntries)
    let tiles = blocks |> Array.map (fun b ->
        let lines = b.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
        let id = int (lines.[0].Replace("Tile ", "").Replace(":", "").Trim())
        let grid = array2D (lines.[1..] |> Array.map (fun s -> s.Trim() |> Seq.toArray))
        { Id = id; Orientations = getOrientations grid })
    let nTiles = tiles.Length
    let gSize = int (Math.Sqrt(float nTiles))
    let tSize = Array2D.length1 tiles.[0].Orientations.[0]
    let used = Array.create nTiles false
    let assembled = Array2D.create gSize gSize (0, 0)
    
    let rec solve idx =
        if idx = nTiles then true
        else
            let r, c = idx / gSize, idx % gSize
            let mutable found = false
            let mutable i = 0
            while i < nTiles && not found do
                if not used.[i] then
                    let mutable o = 0
                    while o < 8 && not found do
                        let curr = tiles.[i].Orientations.[o]
                        let mutable ok = true
                        if r > 0 then
                            let (aI, aO) = assembled.[r-1, c]
                            let above = tiles.[aI].Orientations.[aO]
                            let mutable col = 0
                            while col < tSize && ok do
                                if curr.[0, col] <> above.[tSize-1, col] then ok <- false
                                col <- col + 1
                        if ok && c > 0 then
                            let (lI, lO) = assembled.[r, c-1]
                            let left = tiles.[lI].Orientations.[lO]
                            let mutable row = 0
                            while row < tSize && ok do
                                if curr.[row, 0] <> left.[row, tSize-1] then ok <- false
                                row <- row + 1
                        if ok then
                            used.[i] <- true
                            assembled.[r, c] <- (i, o)
                            if solve (idx + 1) then found <- true else used.[i] <- false
                        o <- o + 1
                i <- i + 1
            found

    if solve 0 then
        let s = tSize - 2
        let dim = gSize * s
        let img = Array2D.create dim dim ' '
        for br in 0 .. gSize - 1 do
            for bc in 0 .. gSize - 1 do
                let (tI, oI) = assembled.[br, bc]
                let g = tiles.[tI].Orientations.[oI]
                for r in 0 .. s - 1 do for c in 0 .. s - 1 do img.[br*s + r, bc*s + c] <- g.[r+1, c+1]
        let mO = [|(0,18);(1,0);(1,5);(1,6);(1,11);(1,12);(1,17);(1,18);(1,19);(2,1);(2,4);(2,7);(2,10);(2,13);(2,16)|]
        let mH, mW = 3, 20
        let mutable d_ = false
        for orient in getOrientations img do
            if not d_ then
                let d = Array2D.length1 orient
                let isM = Array2D.create d d false
                let mutable mC = 0
                for r in 0 .. d - mH do
                    for c in 0 .. d - mW do
                        let mutable m = true
                        for k in 0 .. 14 do
                            let dr, dc = mO.[k]
                            if orient.[r+dr, c+dc] <> '#' then m <- false
                        if m then
                            mC <- mC + 1
                            for k in 0 .. 14 do
                                let dr, dc = mO.[k]
                                isM.[r+dr, c+dc] <- true
                if mC > 0 then
                    let mutable res = 0
                    for r in 0 .. d - 1 do for c in 0 .. d - 1 do if orient.[r,c] = '#' && not isM.[r,c] then res <- res + 1
                    printfn "%d" res
                    d_ <- true
    0
