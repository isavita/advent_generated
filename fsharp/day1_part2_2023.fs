
open System.IO

let digits = [|"zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"|]

let findFirstAndLast (s:string) =
    let rec scan i first last =
        if i = s.Length then 10*first + last
        else
            let c = s.[i]
            let d =
                if System.Char.IsDigit c then int c - int '0'
                else
                    digits
                    |> Array.tryFindIndex (fun d -> s.Substring(i).StartsWith d)
                    |> Option.defaultValue -1
            if d >= 0 then
                let f = if first = 0 then d else first
                scan (i+1) f d
            else scan (i+1) first last
    scan 0 0 0

[<EntryPoint>]
let main _ =
    File.ReadAllLines "input.txt"
    |> Array.sumBy findFirstAndLast
    |> printfn "%d"
    0
