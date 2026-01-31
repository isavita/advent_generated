
open System
open System.IO
open System.Numerics

let lines = File.ReadAllLines "input.txt" |> Array.map (fun s -> s.TrimEnd())
let maxw = lines |> Array.fold (fun m (s:string) -> max m s.Length) 0

let isSep col =
    lines |> Array.forall (fun (s:string) -> col >= s.Length || Char.IsWhiteSpace(s.[col]))

let processBlock sc ec (grand:bigint ref) =
    let mutable op = 0
    let nums = ResizeArray<bigint>()
    for line in lines do
        let e = min (ec+1) line.Length
        if sc < line.Length then
            let seg = line.Substring(sc, e-sc).Trim()
            match seg with
            | "+" -> op <- 1
            | "*" -> op <- 2
            | "" -> ()
            | _ -> nums.Add(BigInteger.Parse seg)
    if nums.Count = 0 then ()
    else
        let acc =
            if op = 1 then nums |> Seq.sum
            elif op = 2 then nums |> Seq.fold (*) 1I
            else nums.[0]
        grand := !grand + acc

let grand = ref 0I
let mutable inb = false
let mutable sc = 0
for x in 0..maxw-1 do
    if not (isSep x) then
        if not inb then inb <- true; sc <- x
    else
        if inb then processBlock sc (x-1) grand; inb <- false
if inb then processBlock sc (maxw-1) grand

printfn "Grand total: %O" !grand
