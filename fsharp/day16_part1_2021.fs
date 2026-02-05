
open System
open System.IO
open System.Text

let hexToBin (hex:string) =
    let sb = StringBuilder()
    for ch in hex do
        let v = Convert.ToInt32(string ch, 16)
        sb.Append(Convert.ToString(v, 2).PadLeft(4, '0')) |> ignore
    sb.ToString()

let rec parse (bin:string) (idx:int) : int * int =
    let ver = ((int bin.[idx] - 48) <<< 2) ||| ((int bin.[idx+1] - 48) <<< 1) ||| (int bin.[idx+2] - 48)
    let typ = ((int bin.[idx+3] - 48) <<< 2) ||| ((int bin.[idx+4] - 48) <<< 1) ||| (int bin.[idx+5] - 48)
    let idx = idx + 6
    if typ = 4 then
        let rec skip i = if bin.[i] = '1' then skip (i+5) else i+5
        ver, skip idx
    else
        let lengthType = int bin.[idx] - 48
        let idx = idx + 1
        if lengthType = 0 then
            let mutable subLen = 0
            let mutable i = idx
            for _ in 0..14 do
                subLen <- (subLen <<< 1) ||| (int bin.[i] - 48)
                i <- i + 1
            let mutable pos = i
            let mutable remain = subLen
            let mutable sum = ver
            while remain > 0 do
                let v, newPos = parse bin pos
                sum <- sum + v
                remain <- remain - (newPos - pos)
                pos <- newPos
            sum, pos
        else
            let mutable cnt = 0
            let mutable i = idx
            for _ in 0..10 do
                cnt <- (cnt <<< 1) ||| (int bin.[i] - 48)
                i <- i + 1
            let mutable pos = i
            let mutable sum = ver
            let mutable c = cnt
            while c > 0 do
                let v, newPos = parse bin pos
                sum <- sum + v
                pos <- newPos
                c <- c - 1
            sum, pos

[<EntryPoint>]
let main _ =
    let hexStr = File.ReadAllText("input.txt").Trim()
    let binStr = hexToBin hexStr
    let result, _ = parse binStr 0
    printfn "%d" result
    0
