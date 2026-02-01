
open System
open System.IO
open System.Text

let reverseSection (arr:int[]) start length =
    let n = arr.Length
    for i = 0 to length/2 - 1 do
        let a = (start + i) % n
        let b = (start + length - 1 - i) % n
        let tmp = arr.[a]
        arr.[a] <- arr.[b]
        arr.[b] <- tmp

let knotHash (input:string) =
    let lengths = Array.zeroCreate (input.Length + 5)
    for i = 0 to input.Length - 1 do
        lengths.[i] <- int input.[i]
    Array.blit [|17;31;73;47;23|] 0 lengths input.Length 5
    let list = Array.init 256 id
    let mutable pos = 0
    let mutable skip = 0
    for _ in 1..64 do
        for l in lengths do
            reverseSection list pos l
            pos <- (pos + l + skip) % 256
            skip <- skip + 1
    let dense = Array.zeroCreate 16
    for i in 0..15 do
        let mutable xor = 0
        for j in 0..15 do
            xor <- xor ^^^ list.[i*16+j]
        dense.[i] <- xor
    let sb = StringBuilder(32)
    for b in dense do
        sb.AppendFormat("{0:x2}", b) |> ignore
    sb.ToString()

let hexToBinary (hexStr:string) =
    let sb = StringBuilder(128)
    for c in hexStr do
        let v = Convert.ToInt32(string c, 16)
        sb.Append(Convert.ToString(v, 2).PadLeft(4, '0')) |> ignore
    sb.ToString()

let rec dfs x y (grid:int[][]) =
    if x >= 0 && x < 128 && y >= 0 && y < 128 && grid.[x].[y] = 1 then
        grid.[x].[y] <- 0
        dfs (x-1) y grid
        dfs (x+1) y grid
        dfs x (y-1) grid
        dfs x (y+1) grid

[<EntryPoint>]
let main _ =
    let keyString = File.ReadAllText("input.txt").Trim()
    let grid = Array.init 128 (fun _ -> Array.zeroCreate 128)
    for i in 0..127 do
        let hash = knotHash (keyString + "-" + string i)
        let bits = hexToBinary hash
        for j in 0..127 do
            if bits.[j] = '1' then grid.[i].[j] <- 1
    let mutable regions = 0
    for i in 0..127 do
        for j in 0..127 do
            if grid.[i].[j] = 1 then
                regions <- regions + 1
                dfs i j grid
    printfn "%d" regions
    0
