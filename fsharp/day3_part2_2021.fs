open System
open System.IO

let filterValues (values: string[]) (criteria: int -> int -> char) : string =
    let mutable current = values
    let bitLen = if current.Length > 0 then current.[0].Length else 0
    let mutable i = 0
    while i < bitLen && current.Length > 1 do
        let zeros = current |> Array.sumBy (fun v -> if v.[i] = '0' then 1 else 0)
        let ones = current.Length - zeros
        let keep = criteria zeros ones
        current <- current |> Array.filter (fun v -> v.[i] = keep)
        i <- i + 1
    current.[0]

[<EntryPoint>]
let main argv =
    let values = File.ReadAllLines "input.txt"
    let oxygenRating = filterValues values (fun zeros ones -> if zeros > ones then '0' else '1')
    let co2Rating = filterValues values (fun zeros ones -> if zeros <= ones then '0' else '1')
    let oxygenInt = System.Convert.ToInt64(oxygenRating, 2)
    let co2Int = System.Convert.ToInt64(co2Rating, 2)
    printfn "%d" (oxygenInt * co2Int)
    0