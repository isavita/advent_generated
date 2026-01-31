
open System
open System.IO

let ways (time:int64) (record:int64) =
    let low = ref 1L
    let high = ref (time - 1L)
    while !low <= !high do
        let mid = (!low + !high) >>> 1
        if mid * (time - mid) > record then high := mid - 1L
        else low := mid + 1L
    int (time - 2L * !low + 1L)

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let parse (s:string) = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.choose (fun t -> match Int64.TryParse t with true,n -> Some n | _ -> None)
    let times = parse lines.[0]
    let dists = parse lines.[1]
    Array.fold2 (fun acc t d -> acc * ways t d) 1 times dists
    |> printfn "%d"
    0
