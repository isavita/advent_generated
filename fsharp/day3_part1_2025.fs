
open System
open System.IO

let calc (s: string) =
    let n = s.Length
    let rec loop d =
        if d < 0 then 0
        else
            let idx = s.IndexOf (char (int '0' + d))
            if idx = -1 || idx = n - 1 then loop (d - 1)
            else
                let mutable max2 = -1
                for i = idx + 1 to n - 1 do
                    let c = s.[i]
                    if Char.IsDigit c then
                        let v = int c - int '0'
                        if v > max2 then max2 <- v
                if max2 <> -1 then d * 10 + max2 else 0
    loop 9

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let total = lines |> Array.sumBy calc
    printfn "%d" total
    0
