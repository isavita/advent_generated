
open System
open System.IO
open System.Numerics

let readLines = File.ReadAllLines("input.txt")

let rec strip (s: string) =
    let t = s.TrimEnd('\r', '\n')
    if t <> s then strip t else t

let lines = readLines |> Array.map strip |> Array.filter (fun s -> s <> "")
if lines.Length = 0 then
    printfn "Grand total: 0"
    exit 0

let maxW = lines |> Array.map (fun s -> s.Length) |> Array.max
let isSep : bool[] = Array.zeroCreate maxW
for x in 0 .. maxW - 1 do
    isSep.[x] <- lines |> Array.forall (fun s -> x >= s.Length || System.Char.IsWhiteSpace(s.[x]))

let mutable grandTotal = BigInteger.Zero
let mutable inBlock = false
let mutable start = 0

for x in 0 .. maxW do
    let isSepX = if x < maxW then isSep.[x] else true
    if not isSepX then
        if not inBlock then
            inBlock <- true
            start <- x
    else
        if inBlock then
            let block = seq {
                for c in start .. x - 1 do
                    let mutable op = '+'
                    let sb = System.Text.StringBuilder()
                    for r in 0 .. lines.Length - 1 do
                        let s = lines.[r]
                        if c < s.Length then
                            let ch = s.[c]
                            if System.Char.IsDigit(ch) then sb.Append(ch) |> ignore
                            elif ch = '+' || ch = '*' then op <- ch
                    if sb.Length > 0 then yield (op, sb.ToString())
            }
            let nums = block |> Seq.toArray
            if nums.Length > 0 then
                let op, _ = nums.[0]
                let res =
                    if op = '*' then
                        nums |> Array.map (fun (_, n) -> BigInteger.Parse n) |> Array.fold (*) BigInteger.One
                    else
                        nums |> Array.map (fun (_, n) -> BigInteger.Parse n) |> Array.fold (+) BigInteger.Zero
                grandTotal <- grandTotal + res
            inBlock <- false

printfn "Grand total: %A" grandTotal
