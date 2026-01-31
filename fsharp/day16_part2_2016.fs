
open System
open System.IO

[<EntryPoint>]
let main _ =
    let DISK_LENGTH = 35651584
    let init = File.ReadAllText("input.txt").Trim().ToCharArray()
    let data = Array.create<char> DISK_LENGTH '0'
    Array.blit init 0 data 0 init.Length
    let mutable cur = init.Length
    while cur < DISK_LENGTH do
        data.[cur] <- '0'
        let mutable i = 0
        while i < cur && cur + 1 + i < DISK_LENGTH do
            data.[cur + 1 + i] <- (if data.[cur - 1 - i] = '0' then '1' else '0')
            i <- i + 1
        cur <- cur * 2 + 1
    let mutable len = DISK_LENGTH
    while len % 2 = 0 do
        for i = 0 to (len/2) - 1 do
            data.[i] <- (if data.[2*i] = data.[2*i+1] then '1' else '0')
        len <- len / 2
    let result = String(data, 0, len)
    printfn "%s" result
    0
