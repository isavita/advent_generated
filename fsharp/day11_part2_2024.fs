
open System
open System.Collections.Generic
open System.IO

let inline (>>=) x f = f x

let readInput () =
    File.ReadAllText("input.txt").Split(' ', StringSplitOptions.RemoveEmptyEntries)

let rec step (memo: Dictionary<string*int,int64>) stone rem =
    let key = stone,rem
    if memo.ContainsKey(key) then memo.[key]
    elif rem = 0 then 1L
    else
        let res =
            if stone = "0" then step memo "1" (rem-1)
            elif stone.Length % 2 = 0 then
                let half = stone.Length / 2
                let left = stone.[0..half-1] |> fun s -> s.TrimStart('0') |> function "" -> "0" | x -> x
                let right = stone.[half..] |> fun s -> s.TrimStart('0') |> function "" -> "0" | x -> x
                step memo left (rem-1) + step memo right (rem-1)
            else
                let mutable big = 0L
                for ch in stone do
                    big <- big * 10L + int64 ch - 48L
                step memo (string (big * 2024L)) (rem-1)
        memo.[key] <- res
        res

[<EntryPoint>]
let main _ =
    readInput()
    |> Array.sumBy (fun s -> step (Dictionary<_,_>()) s 75)
    |> printfn "%d"
    0
