open System
open System.IO
open System.Security.Cryptography
open System.Text

let getMd5Hash (input:string) =
    use md5 = MD5.Create()
    let bytes = Encoding.ASCII.GetBytes(input)
    let hashBytes = md5.ComputeHash(bytes)
    let sb = StringBuilder()
    for b in hashBytes do sb.Append(b.ToString("x2")) |> ignore
    sb.ToString()

let findTriplet (hash:string) =
    let rec loop i =
        if i > hash.Length - 3 then None
        elif hash.[i] = hash.[i+1] && hash.[i] = hash.[i+2] then Some hash.[i]
        else loop (i+1)
    loop 0

[<EntryPoint>]
let main _ =
    let salt = File.ReadAllText("input.txt").Trim()
    let mutable keys = 0
    let mutable index = 0
    while keys < 64 do
        let hash = getMd5Hash (salt + string index)
        match findTriplet hash with
        | None -> ()
        | Some c ->
            let quintuple = string c |> fun s -> s.PadRight(5, c)
            let mutable found = false
            for i in 1 .. 1000 do
                if not found then
                    let nextHash = getMd5Hash (salt + string (index + i))
                    if nextHash.Contains(quintuple) then
                        found <- true
                        keys <- keys + 1
        index <- index + 1
    printfn "%d" (index - 1)
    0