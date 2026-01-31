
open System
open System.IO

type Num = { Pos: int; Val: int64 }

let mix (nums: Num[]) =
    let n = nums.Length - 1
    for i in 0 .. nums.Length - 1 do
        let oldPos = nums.[i].Pos
        let move = int64 nums.[i].Pos
        let newPos = ((oldPos + int (nums.[i].Val % int64 n) + n) % n)
        if oldPos < newPos then
            for j in 0 .. nums.Length - 1 do
                if nums.[j].Pos > oldPos && nums.[j].Pos <= newPos then
                    nums.[j] <- { nums.[j] with Pos = nums.[j].Pos - 1 }
        elif newPos < oldPos then
            for j in 0 .. nums.Length - 1 do
                if nums.[j].Pos >= newPos && nums.[j].Pos < oldPos then
                    nums.[j] <- { nums.[j] with Pos = nums.[j].Pos + 1 }
        nums.[i] <- { nums.[i] with Pos = newPos }

let coords (nums: Num[]) =
    let zeroPos = nums |> Array.find (fun x -> x.Val = 0L) |> fun x -> x.Pos
    let l = nums.Length
    [|1000; 2000; 3000|]
    |> Array.sumBy (fun offset -> nums |> Array.find (fun x -> x.Pos = (zeroPos + offset) % l) |> fun x -> x.Val)

[<EntryPoint>]
let main _ =
    let nums =
        File.ReadAllLines("input.txt")
        |> Array.mapi (fun i line -> { Pos = i; Val = int64 (int line) })
    mix nums
    printfn "%d" (coords nums)
    0
