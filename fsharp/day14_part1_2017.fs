
open System
open System.IO
open System.Text

let reverse (arr:int[]) start len =
    let n = 256
    for i in 0 .. (len/2 - 1) do
        let i1 = (start + i) % n
        let i2 = (start + len - 1 - i) % n
        let tmp = arr.[i1]
        arr.[i1] <- arr.[i2]
        arr.[i2] <- tmp

let knotHash (input:string) =
    let lengths = Array.append (input |> Seq.map int |> Seq.toArray) [|17;31;73;47;23|]
    let list = Array.init 256 id
    let mutable pos = 0
    let mutable skip = 0
    for _ in 0 .. 63 do
        for l in lengths do
            if l > 0 then reverse list pos l
            pos <- (pos + l + skip) % 256
            skip <- skip + 1
    let dense =
        [| for i in 0 .. 15 ->
            let mutable x = 0
            for j in 0 .. 15 do x <- x ^^^ list.[i*16 + j]
            x |]
    let sb = StringBuilder()
    for v in dense do sb.AppendFormat("{0:x2}", v) |> ignore
    sb.ToString()

let bits = [|0;1;1;2;1;2;2;3;1;2;2;3;2;3;3;4|]

[<EntryPoint>]
let main _ =
    let key = File.ReadAllText("input.txt").Trim()
    let mutable total = 0
    for i in 0 .. 127 do
        let rowKey = sprintf "%s-%d" key i
        let hash = knotHash rowKey
        for c in hash do
            total <- total + bits.[Convert.ToInt32(c.ToString(),16)]
    printfn "%d" total
    0
