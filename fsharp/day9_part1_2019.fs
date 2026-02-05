
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let mem = Dictionary<int64, int64>()
    let content = File.ReadAllText("input.txt")
    let raw = content.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    raw |> Array.iteri (fun i s -> mem.[int64 i] <- Int64.Parse(s.Trim()))

    let read a = 
        let mutable v = 0L
        if mem.TryGetValue(a, &v) then v else 0L

    let mutable ip, rb, last = 0L, 0L, 0L
    let mutable run = true
    while run do
        let cmd = read ip
        let op, modes = cmd % 100L, cmd / 100L
        let get n =
            let m = (modes / (match n with 1 -> 1L | 2 -> 10L | _ -> 100L)) % 10L
            let v = read (ip + int64 n)
            match m with 0L -> read v | 1L -> v | 2L -> read (rb + v) | _ -> 0L
        let addr n =
            let m = (modes / (match n with 1 -> 1L | 2 -> 10L | _ -> 100L)) % 10L
            let v = read (ip + int64 n)
            if m = 2L then rb + v else v
        match op with
        | 1L -> mem.[addr 3] <- get 1 + get 2; ip <- ip + 4L
        | 2L -> mem.[addr 3] <- get 1 * get 2; ip <- ip + 4L
        | 3L -> mem.[addr 1] <- 1L; ip <- ip + 2L
        | 4L -> last <- get 1; ip <- ip + 2L
        | 5L -> if get 1 <> 0L then ip <- get 2 else ip <- ip + 3L
        | 6L -> if get 1 = 0L then ip <- get 2 else ip <- ip + 3L
        | 7L -> mem.[addr 3] <- (if get 1 < get 2 then 1L else 0L); ip <- ip + 4L
        | 8L -> mem.[addr 3] <- (if get 1 = get 2 then 1L else 0L); ip <- ip + 4L
        | 9L -> rb <- rb + get 1; ip <- ip + 2L
        | 99L -> run <- false
        | _ -> run <- false
    printfn "%d" last
    0
