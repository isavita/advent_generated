
open System
open System.IO
open System.Text
open System.Security.Cryptography

[<EntryPoint>]
let main _ =
    let doorID = File.ReadAllText("input.txt").Trim()
    use md5 = MD5.Create()
    let password = Array.create 8 ' '
    let filled = Array.create 8 false
    let mutable count = 0
    let mutable index = 0L
    let hex = "0123456789abcdef"
    while count < 8 do
        let s = doorID + string index
        let bytes = Encoding.ASCII.GetBytes(s)
        let hash = md5.ComputeHash(bytes)
        if hash.[0] = 0uy && hash.[1] = 0uy && (hash.[2] &&& 0xF0uy) = 0uy then
            let pos = int (hash.[2] &&& 0x0Fuy)
            if pos < 8 && not filled.[pos] then
                password.[pos] <- hex.[int ((hash.[3] &&& 0xF0uy) >>> 4)]
                filled.[pos] <- true
                count <- count + 1
        index <- index + 1L
    Console.WriteLine(String(password))
    0
