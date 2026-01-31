
open System
open System.IO

let totalRows = 400000

let isTrap (l:bool) (c:bool) (r:bool) =
    (l && c && not r) ||
    (c && r && not l) ||
    (l && not c && not r) ||
    (r && not c && not l)

[<EntryPoint>]
let main _ =
    // read first row, remove newline
    let firstLine = File.ReadAllText("input.txt").Trim()
    let n = firstLine.Length

    // current row: true = trap ('^'), false = safe ('.')
    let cur = Array.init n (fun i -> firstLine.[i] = '^')
    let mutable safeCount = cur |> Array.filter (not) |> Array.length

    let next = Array.zeroCreate<bool> n

    for _ = 1 to totalRows-1 do
        for i = 0 to n-1 do
            let l = if i = 0 then false else cur.[i-1]
            let c = cur.[i]
            let r = if i = n-1 then false else cur.[i+1]
            next.[i] <- isTrap l c r
        // count safe tiles in the new row
        safeCount <- safeCount + (next |> Array.filter (not) |> Array.length)
        // swap buffers
        Array.Copy(next, cur, n)

    printfn "%d" safeCount
    0
