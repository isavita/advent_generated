
open System
open System.IO
open System.Collections.Generic

type State = { cost:int; r:int; c:int; dir:int }

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let rows = lines.Length
    let cols = if rows = 0 then 0 else lines.[0].Length
    let mutable sr = -1
    let mutable sc = -1
    let mutable er = -1
    let mutable ec = -1
    for i in 0 .. rows-1 do
        for j in 0 .. cols-1 do
            match lines.[i].[j] with
            | 'S' -> sr <- i; sc <- j
            | 'E' -> er <- i; ec <- j
            | _   -> ()
    if sr = -1 || er = -1 then
        1 |> ignore
    let dr = [|0;1;0;-1|]
    let dc = [|1;0;-1;0|]
    let visited = Array3D.create rows cols 4 false
    let pq = PriorityQueue<State,int>()
    pq.Enqueue({cost=0; r=sr; c=sc; dir=0},0)
    let rec loop () =
        if pq.Count = 0 then
            Console.Error.WriteLine("End not reachable")
            1
        else
            let cur = pq.Dequeue()
            let r,c,d = cur.r,cur.c,cur.dir
            if visited.[r,c,d] then loop()
            else
                visited.[r,c,d] <- true
                if r = er && c = ec then
                    printfn "%d" cur.cost
                    0
                else
                    let nr = r + dr.[d]
                    let nc = c + dc.[d]
                    if nr>=0 && nr<rows && nc>=0 && nc<cols && lines.[nr].[nc] <> '#' && not visited.[nr,nc,d] then
                        pq.Enqueue({cost=cur.cost+1; r=nr; c=nc; dir=d}, cur.cost+1)
                    let dCw = (d+1) &&& 3
                    if not visited.[r,c,dCw] then
                        pq.Enqueue({cost=cur.cost+1000; r=r; c=c; dir=dCw}, cur.cost+1000)
                    let dCc = (d+3) &&& 3
                    if not visited.[r,c,dCc] then
                        pq.Enqueue({cost=cur.cost+1000; r=r; c=c; dir=dCc}, cur.cost+1000)
                    loop()
    loop()
