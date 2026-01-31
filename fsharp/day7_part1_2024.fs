
open System
open System.IO

let evaluate (target:int64) (nums:int64[]) =
    let n = nums.Length
    if n = 1 then nums.[0] = target else
    let masks = 1 <<< (n-1)
    let mutable ok = false
    let mutable m = 0
    while not ok && m < masks do
        let mutable acc = nums.[0]
        for i = 1 to n-1 do
            if (m >>> (i-1)) &&& 1 = 0 then acc <- acc + nums.[i]
            else acc <- acc * nums.[i]
        ok <- acc = target
        m <- m + 1
    ok

let solve (line:string) =
    let parts = line.Split(':')
    let target = int64 parts.[0]
    let nums =
        parts.[1].Trim().Split(' ')
        |> Array.map int64
    if evaluate target nums then target else 0L

[<EntryPoint>]
let main _ =
    File.ReadAllLines("input.txt")
    |> Array.map solve
    |> Array.sum
    |> printfn "%d"
    0
