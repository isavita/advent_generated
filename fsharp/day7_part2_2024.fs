
open System
open System.IO

let concat (a:int64) (b:int64) =
    let mutable m = 1L
    let mutable t = b
    while t > 0L do
        m <- m * 10L
        t <- t / 10L
    a * m + b

let check (target:int64) (nums:int64[]) =
    let n = nums.Length - 1
    let combos = 1 <<< (n * 2)
    let mutable ok = false
    let mutable mask = 0
    while mask < combos && not ok do
        let mutable cur = nums.[0]
        let mutable m = mask
        for i = 0 to n-1 do
            let op = m &&& 3
            m <- m >>> 2
            let nxt = nums.[i+1]
            match op with
            | 0 -> cur <- cur + nxt
            | 1 -> cur <- cur * nxt
            | _ -> cur <- concat cur nxt
        if cur = target then ok <- true
        mask <- mask + 1
    ok

[<EntryPoint>]
let main _ =
    let mutable sum = 0L
    for line in File.ReadLines "input.txt" do
        let parts = line.Split([|':'|], StringSplitOptions.RemoveEmptyEntries)
        let target = Int64.Parse(parts.[0])
        let nums = parts.[1].Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map Int64.Parse
        if check target nums then sum <- sum + target
    printfn "%d" sum
    0
