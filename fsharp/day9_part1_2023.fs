
open System.IO
let allZeros (a:int[]) =
    let rec loop i = i >= a.Length || (a.[i] = 0 && loop (i+1))
    loop 0
let diffs (a:int[]) =
    Array.init (a.Length-1) (fun i -> a.[i+1] - a.[i])
let predict (line:string) =
    let nums = line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Array.map int
    let tri = System.Collections.Generic.List<int[]>(10)
    tri.Add nums
    while not (allZeros tri.[tri.Count-1]) do
        tri.Add (diffs tri.[tri.Count-1])
    tri |> Seq.sumBy (fun a -> a.[a.Length-1])
[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.sumBy predict
    |> printfn "%d"
    0
