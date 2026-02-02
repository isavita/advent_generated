
open System
open System.IO

let reg (r: char) = int r - int 'w'

let atoiSafe (s: string) = int s

let readInput () =
    let lines = File.ReadAllLines("input.txt")
    let k = Array.init 14 (fun _ -> 0)
    let l = Array.init 14 (fun _ -> 0)
    let m = Array.init 14 (fun _ -> 0)
    for i = 0 to lines.Length - 1 do
        let line = lines[i]
        if i % 18 = 4 then
            let v = line.Split(' ').[2] |> atoiSafe
            l.[i / 18] <- v
        elif i % 18 = 5 then
            let v = line.Split(' ').[2] |> atoiSafe
            k.[i / 18] <- v
        elif i % 18 = 15 then
            let v = line.Split(' ').[2] |> atoiSafe
            m.[i / 18] <- v
    (k, l, m)

let manual (s: string) =
    let k = [|11; 14; 10; 14; -8; 14; -11; 10; -6; -9; 12; -5; -4; -9|]
    let l = [|1; 1; 1; 1; 26; 1; 26; 1; 26; 26; 1; 26; 26; 26|]
    let m = [|7; 8; 16; 8; 3; 12; 1; 8; 8; 14; 4; 14; 15; 6|]
    let w = s.ToCharArray() |> Array.map (fun c -> int c - int '0')
    let mutable z = 0
    for i = 0 to 13 do
        let x = z % 26 + k.[i]
        if l.[i] = 1 then
            z <- z * 26 + w.[i] + m.[i]
        else
            z <- z / 26
            if x <> w.[i] then z <- z * 26 + w.[i] + m.[i]
    z

let main () =
    let (k, l, m) = readInput ()
    let constraints = Array.init 14 (fun _ -> Array.init 2 (fun _ -> 0))
    let stack = Array.init 14 (fun _ -> 0)
    let mutable stackPtr = 0
    for i = 0 to 13 do
        if l.[i] = 1 then
            stack.[stackPtr] <- i
            stackPtr <- stackPtr + 1
        else
            let pop = stack.[stackPtr - 1]
            stackPtr <- stackPtr - 1
            constraints.[pop].[0] <- i
            constraints.[pop].[1] <- m.[pop] + k.[i]

    let min = Array.init 14 (fun _ -> 0)
    for i = 0 to 13 do
        if constraints.[i].[0] = 0 && constraints.[i].[1] = 0 then () else
        let mutable vmin = 1
        while vmin + constraints.[i].[1] < 1 do vmin <- vmin + 1
        min.[i] <- vmin
        min.[constraints.[i].[0]] <- vmin + constraints.[i].[1]

    let mutable n = 0L
    for i = 0 to 13 do
        n <- n * 10L + int64 min.[i]
    printfn "%d" n

main ()
