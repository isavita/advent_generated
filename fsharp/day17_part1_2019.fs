
open System
open System.IO
open System.Text

[<EntryPoint>]
let main _ =
    let input = File.ReadAllText("input.txt").Trim()
    let program = input.Split(',') |> Array.map int64
    let mem = Array.zeroCreate 10000
    Array.blit program 0 mem 0 program.Length
    
    let mutable ip, rb = 0L, 0L
    let sb = StringBuilder()

    let get i m =
        match m with
        | 0L -> mem.[int mem.[int i]]
        | 1L -> mem.[int i]
        | 2L -> mem.[int (rb + mem.[int i])]
        | _ -> 0L

    let set i m v =
        match m with
        | 0L -> mem.[int mem.[int i]] <- v
        | 2L -> mem.[int (rb + mem.[int i])] <- v
        | _ -> ()

    let mutable halt = false
    while not halt do
        let f = mem.[int ip]
        let op = f % 100L
        let m1 = (f / 100L) % 10L
        let m2 = (f / 1000L) % 10L
        let m3 = (f / 10000L) % 10L
        match op with
        | 1L -> set (ip + 3L) m3 (get (ip + 1L) m1 + get (ip + 2L) m2); ip <- ip + 4L
        | 2L -> set (ip + 3L) m3 (get (ip + 1L) m1 * get (ip + 2L) m2); ip <- ip + 4L
        | 3L -> set (ip + 1L) m1 0L; ip <- ip + 2L
        | 4L -> sb.Append(char(get (ip + 1L) m1)) |> ignore; ip <- ip + 2L
        | 5L -> if get (ip + 1L) m1 <> 0L then ip <- get (ip + 2L) m2 else ip <- ip + 3L
        | 6L -> if get (ip + 1L) m1 = 0L then ip <- get (ip + 2L) m2 else ip <- ip + 3L
        | 7L -> set (ip + 3L) m3 (if get (ip + 1L) m1 < get (ip + 2L) m2 then 1L else 0L); ip <- ip + 4L
        | 8L -> set (ip + 3L) m3 (if get (ip + 1L) m1 = get (ip + 2L) m2 then 1L else 0L); ip <- ip + 4L
        | 9L -> rb <- rb + get (ip + 1L) m1; ip <- ip + 2L
        | 99L -> halt <- true
        | _ -> halt <- true

    let grid = sb.ToString().Trim().Split('\n') |> Array.filter (fun s -> s.Length > 0)
    let isS y x =
        if y < 0 || y >= grid.Length || x < 0 || x >= grid.[0].Length then false
        else 
            let c = grid.[y].[x]
            c = '#' || c = '^' || c = 'v' || c = '<' || c = '>'

    let mutable ans = 0
    for y in 1 .. grid.Length - 2 do
        for x in 1 .. grid.[0].Length - 2 do
            if isS y x && isS (y - 1) x && isS (y + 1) x && isS y (x - 1) && isS y (x + 1) then
                ans <- ans + x * y
    printfn "%d" ans
    0

