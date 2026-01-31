
open System
open System.IO
open System.Text
open System.Security.Cryptography
open System.Collections.Generic

type Point = { x:int; y:int; path:string }

[<EntryPoint>]
let main _ =
    let passcode = File.ReadAllText("input.txt").Trim()
    let q = Queue<Point>()
    q.Enqueue({ x = 0; y = 0; path = "" })
    let mutable longest = 0
    let dirs = [| ('U', 0, -1); ('D', 0, 1); ('L', -1, 0); ('R', 1, 0) |]
    use md5 = MD5.Create()
    while q.Count > 0 do
        let cur = q.Dequeue()
        if cur.x = 3 && cur.y = 3 then
            if cur.path.Length > longest then longest <- cur.path.Length
        else
            let input = passcode + cur.path
            let hash = md5.ComputeHash(Encoding.ASCII.GetBytes(input))
            let hex = BitConverter.ToString(hash).Replace("-", "").ToLower()
            for i in 0 .. 3 do
                let c = hex.[i]
                if c >= 'b' && c <= 'f' then
                    let dir, dx, dy = dirs.[i]
                    let nx, ny = cur.x + dx, cur.y + dy
                    if nx >= 0 && nx < 4 && ny >= 0 && ny < 4 then
                        q.Enqueue({ x = nx; y = ny; path = cur.path + string dir })
    printfn "%d" longest
    0
