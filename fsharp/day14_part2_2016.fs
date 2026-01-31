
open System
open System.IO
open System.Text
open System.Security.Cryptography
open System.Collections.Generic

let md5Hex (s:string) =
    let bytes = Encoding.ASCII.GetBytes(s)
    use md5 = MD5.Create()
    let hash = md5.ComputeHash(bytes)
    BitConverter.ToString(hash).Replace("-", "").ToLower()

let stretched (salt:string) (idx:int) =
    let mutable h = md5Hex (salt + string idx)
    for _ in 1 .. 2016 do h <- md5Hex h
    h

[<EntryPoint>]
let main _ =
    let salt = File.ReadAllLines("input.txt").[0].Trim()
    let cache = Dictionary<int,string>()
    let get idx =
        match cache.TryGetValue(idx) with
        | true, v -> v
        | _ ->
            let v = stretched salt idx
            cache.[idx] <- v
            v
    let rec findTriplet (s:string) i =
        if i > s.Length - 3 then None
        elif s.[i] = s.[i+1] && s.[i] = s.[i+2] then Some s.[i]
        else findTriplet s (i+1)
    let hasQuintuplet (s:string) c =
        let pat = String(Array.create 5 c)
        s.Contains(pat)
    let mutable found = 0
    let mutable index = 0
    while found < 64 do
        let h = get index
        match findTriplet h 0 with
        | Some c ->
            let mutable ok = false
            for j in 1 .. 1000 do
                if not ok && hasQuintuplet (get (index + j)) c then
                    ok <- true
            if ok then found <- found + 1
        | None -> ()
        if found = 64 then printfn "%d" index
        index <- index + 1
    0
