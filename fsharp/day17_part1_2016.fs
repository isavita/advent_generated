open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Collections.Generic

type Point = { x:int; y:int; path:string }

let md5Hash (input:string) =
    use md5 = MD5.Create()
    let bytes = Encoding.ASCII.GetBytes(input)
    let hashBytes = md5.ComputeHash(bytes)
    let sb = StringBuilder()
    for b in hashBytes do sb.Append(b.ToString("x2")) |> ignore
    sb.ToString()

let openDoors (pass:string) (path:string) =
    let h = md5Hash (pass + path)
    let dirs = [ 'U'; 'D'; 'L'; 'R' ]
    let firstFour = h.Substring(0,4)
    let rec collect acc idx =
        if idx = 4 then List.rev acc
        else
            let c = firstFour.[idx]
            let dir = dirs.[idx]
            let acc' = if c >= 'b' && c <= 'f' then dir::acc else acc
            collect acc' (idx+1)
    collect [] 0

let findShortestPath pass =
    let q = Queue<Point>()
    q.Enqueue({x=0; y=0; path=""})
    let rec bfs () =
        if q.Count = 0 then None
        else
            let p = q.Dequeue()
            if p.x = 3 && p.y = 3 then Some p.path
            else
                for dir in openDoors pass p.path do
                    let nx, ny =
                        match dir with
                        | 'U' -> p.x, p.y - 1
                        | 'D' -> p.x, p.y + 1
                        | 'L' -> p.x - 1, p.y
                        | 'R' -> p.x + 1, p.y
                        | _ -> p.x, p.y
                    if nx >= 0 && nx < 4 && ny >= 0 && ny < 4 then
                        q.Enqueue({x=nx; y=ny; path=p.path + string dir})
                bfs ()
    bfs ()

[<EntryPoint>]
let main argv =
    let pass = File.ReadAllText("input.txt").Trim()
    let result = findShortestPath pass
    match result with
    | Some s -> printfn "%s" s
    | None -> printfn "No path found"
    0