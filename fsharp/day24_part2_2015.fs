
open System
open System.IO

[<EntryPoint>]
let main _ =
    let packages = File.ReadAllLines("input.txt") |> Array.map int64
    let target = Array.sum packages / 4L
    let n = packages.Length
    let mutable bestLen = Int32.MaxValue
    let mutable bestQE = Int64.MaxValue
    let maxMask = 1 <<< n
    for mask = 1 to maxMask - 1 do
        let mutable w = 0L
        let mutable qe = 1L
        let mutable c = 0
        for i = 0 to n - 1 do
            if (mask &&& (1 <<< i)) <> 0 then
                w <- w + packages.[i]
                qe <- qe * packages.[i]
                c <- c + 1
        if w = target && c <= bestLen then
            let rem = 
                [| for i = 0 to n - 1 do
                       if (mask &&& (1 <<< i)) = 0 then yield packages.[i] |]
            let rlen = rem.Length
            let mutable ok = false
            let max2 = 1 <<< rlen
            let mutable m2 = 1
            while m2 < max2 && not ok do
                let mutable w2 = 0L
                for i = 0 to rlen - 1 do
                    if (m2 &&& (1 <<< i)) <> 0 then w2 <- w2 + rem.[i]
                if w2 = target then
                    let mutable m3 = 1
                    while m3 < max2 && not ok do
                        if (m2 &&& m3) = 0 then
                            let mutable w3 = 0L
                            for i = 0 to rlen - 1 do
                                if (m3 &&& (1 <<< i)) <> 0 then w3 <- w3 + rem.[i]
                            if w3 = target then ok <- true
                        m3 <- m3 + 1
                m2 <- m2 + 1
            if ok && (c < bestLen || qe < bestQE) then
                bestLen <- c
                bestQE <- qe
    printfn "%d" bestQE
    0
