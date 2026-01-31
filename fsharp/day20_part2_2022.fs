
open System
open System.IO

type Num = { Pos:int; Value:int64 }

let mix (n:int) (arr:Num[]) =
    for i in 0..n-1 do
        let oldPos = arr.[i].Pos
        let newPos = int64 oldPos + arr.[i].Value
        let newPos = (newPos % int64 (n-1) + int64 (n-1)) % int64 (n-1) |> int
        if oldPos < newPos then
            for j in 0..n-1 do
                if arr.[j].Pos > oldPos && arr.[j].Pos <= newPos then
                    arr.[j] <- { arr.[j] with Pos = arr.[j].Pos - 1 }
        elif newPos < oldPos then
            for j in 0..n-1 do
                if arr.[j].Pos >= newPos && arr.[j].Pos < oldPos then
                    arr.[j] <- { arr.[j] with Pos = arr.[j].Pos + 1 }
        arr.[i] <- { arr.[i] with Pos = newPos }

let coords (n:int) (arr:Num[]) =
    let zeroPos = arr |> Array.find (fun x -> x.Value = 0L) |> fun x -> x.Pos
    [|1000;2000;3000|]
    |> Array.sumBy (fun off -> arr |> Array.find (fun x -> x.Pos = (zeroPos + off) % n) |> fun x -> x.Value)

[<EntryPoint>]
let main _ =
    let nums =
        File.ReadAllLines "input.txt"
        |> Array.mapi (fun i line -> { Pos=i; Value=int64 line * 811589153L })
    let n = nums.Length
    for _ in 1..10 do mix n nums
    printfn "%d" (coords n nums)
    0
