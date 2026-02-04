
open System
open System.IO

let hexToBin (s:string) =
    s
    |> Seq.map (fun c ->
        Convert.ToInt32(c.ToString(), 16).ToString("b").PadLeft(4, '0')
    )
    |> String.concat ""

let rec parseLiteral (bin:string) (idx:int) (value:int64) =
    let group = bin.Substring(idx, 5)
    let valPart = Convert.ToInt64(group.Substring(1), 2)
    let newValue = (value <<< 4) ||| valPart
    if group.[0] = '0' then
        (newValue, idx + 5)
    else
        parseLiteral bin (idx + 5) newValue

let rec parsePacket (bin:string) (idx:int) : (int * int * int64) =
    let version = Convert.ToInt32(bin.Substring(idx, 3), 2)
    let typeId = Convert.ToInt32(bin.Substring(idx + 3, 3), 2)
    let mutable pos = idx + 6

    if typeId = 4 then
        let (value, newPos) = parseLiteral bin pos 0L
        (version, newPos, value)
    else
        let lenType = bin.[pos]
        pos <- pos + 1
        let mutable values = []

        if lenType = '0' then
            let totalLen = Convert.ToInt32(bin.Substring(pos, 15), 2)
            pos <- pos + 15
            let endPos = pos + totalLen
            while pos < endPos do
                let (_, newPos, v) = parsePacket bin pos
                values <- v :: values
                pos <- newPos
        else
            let subCount = Convert.ToInt32(bin.Substring(pos, 11), 2)
            pos <- pos + 11
            for _ in 1 .. subCount do
                let (_, newPos, v) = parsePacket bin pos
                values <- v :: values
                pos <- newPos

        let valuesRev = List.rev values
        let result =
            match typeId with
            | 0 -> List.sum valuesRev |> int64
            | 1 -> List.fold (*) 1L valuesRev
            | 2 -> List.min valuesRev
            | 3 -> List.max valuesRev
            | 5 -> if valuesRev.[0] > valuesRev.[1] then 1L else 0L
            | 6 -> if valuesRev.[0] < valuesRev.[1] then 1L else 0L
            | 7 -> if valuesRev.[0] = valuesRev.[1] then 1L else 0L
            | _ -> failwithf "unknown type %d" typeId
        (version, pos, result)

[<EntryPoint>]
let main argv =
    let hex = File.ReadAllText("input.txt").Trim()
    let bin = hexToBin hex
    let (_, _, value) = parsePacket bin 0
    printfn "%d" value
    0
