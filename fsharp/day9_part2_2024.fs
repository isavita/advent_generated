
open System
open System.IO

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").TrimEnd('\n', '\r')
    let n = input.Length
    let totalSize =
        let mutable s = 0
        for i = 0 to n - 1 do
            let c = input.[i]
            if c >= '0' && c <= '9' then s <- s + (int c - int '0')
        s
    if totalSize = 0 then
        printfn "0"
        0
    else
        let fileCount = (n + 1) / 2
        let disk = Array.zeroCreate<int> totalSize
        let start = Array.zeroCreate<int> fileCount
        let finish = Array.zeroCreate<int> fileCount
        let mutable pos = 0
        let mutable fid = 0
        for i = 0 to n - 1 do
            let len = int input.[i] - int '0'
            if (i &&& 1) = 0 then
                start.[fid] <- pos
                finish.[fid] <- pos + len - 1
                for j = 0 to len - 1 do disk.[pos + j] <- fid
                fid <- fid + 1
            else
                for j = 0 to len - 1 do disk.[pos + j] <- -1
            pos <- pos + len
        for i = fileCount - 1 downto 0 do
            let len = finish.[i] - start.[i] + 1
            let mutable best = -1
            let mutable cur = 0
            let mutable j = 0
            while j < start.[i] && cur < len do
                if disk.[j] = -1 then
                    if cur = 0 then best <- j
                    cur <- cur + 1
                else
                    cur <- 0
                    best <- -1
                j <- j + 1
            if best <> -1 && cur = len then
                for k = 0 to len - 1 do disk.[best + k] <- i
                for k = start.[i] to finish.[i] do disk.[k] <- -1
        let mutable checksum = 0L
        for i = 0 to totalSize - 1 do
            let v = disk.[i]
            if v <> -1 then checksum <- checksum + int64 i * int64 v
        printfn "%d" checksum
        0
