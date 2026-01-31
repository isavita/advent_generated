
open System
open System.IO
open System.Numerics

let pow10 = Array.init 21 (fun i -> pown 10I i)

let parseBigint (s:string) = BigInteger.Parse s

let readInput() =
    let txt = File.ReadAllText("input.txt").Replace("\r"," ").Replace("\n"," ").Replace(" ",",")
    txt.Split([|','|],StringSplitOptions.RemoveEmptyEntries)

let seedIds =
    readInput()
    |> Array.collect (fun seg ->
        let d = seg.IndexOf '-'
        if d<0 then failwith "bad range"
        let start = parseBigint (seg.Substring(0,d))
        let last  = parseBigint (seg.Substring(d+1))
        let start,endt = if start>last then last,start else start,last
        [| for k=1 to 10 do
               let mult = pow10.[k] + 1I
               let minS = pow10.[k-1]
               let maxS = pow10.[k] - 1I
               let sMin = (start + mult - 1I) / mult
               let sMax = endt / mult
               let sMin = max sMin minS
               let sMax = min sMax maxS
               if sMin <= sMax then
                   for seed=sMin to sMax do yield seed * mult |] )
    |> Array.distinct
    |> Array.sum

printfn "%A" seedIds
