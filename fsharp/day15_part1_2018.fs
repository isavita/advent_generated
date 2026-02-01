
open System
open System.IO
open System.Collections.Generic

let HP,Power = 200,3
let Dirs = [| (0,-1); (-1,0); (1,0); (0,1) |]

type Unit = { mutable X:int; mutable Y:int; mutable Hp:int; Atk:int; Type:char }
    with member this.Dead = this.Hp<=0

let readInput() =
    let lines = File.ReadAllLines "input.txt"
    let h,w = lines.Length, lines.[0].Length
    let grid = Array2D.init h w (fun y x -> lines.[y].[x])
    let units = ResizeArray()
    for y in 0..h-1 do
        for x in 0..w-1 do
            let c = grid.[y,x]
            if c='E' || c='G' then
                units.Add({X=x;Y=y;Hp=HP;Atk=Power;Type=c})
    grid,units

let getDists (sx,sy) (grid:char[,]) =
    let h,w = grid.GetLength 0, grid.GetLength 1
    let dist = Dictionary()
    let q = Queue()
    dist.[(sx,sy)] <- 0
    q.Enqueue (sx,sy)
    while q.Count>0 do
        let x,y = q.Dequeue()
        let d = dist.[(x,y)]
        for dx,dy in Dirs do
            let nx,ny = x+dx,y+dy
            if grid.[ny,nx]='.' && not (dist.ContainsKey (nx,ny)) then
                dist.[(nx,ny)] <- d+1
                q.Enqueue (nx,ny)
    dist

let move (u:Unit) (grid:char[,]) (targets:Unit list) =
    let h,w = grid.GetLength 0, grid.GetLength 1
    let inRange = HashSet()
    for t in targets do
        for dx,dy in Dirs do
            let nx,ny = t.X+dx,t.Y+dy
            if grid.[ny,nx]='.' then inRange.Add (nx,ny) |> ignore
    let dists = getDists (u.X,u.Y) grid
    let reachable =
        inRange
        |> Seq.choose (fun p -> if dists.ContainsKey p then Some (dists.[p],snd p,fst p) else None)
        |> Seq.sort
        |> Seq.toList
    if reachable.IsEmpty then () else
        let chosen = reachable.Head
        let stepD = getDists (chosen.Item3,chosen.Item2) grid
        let bestStep =
            Dirs
            |> Array.choose (fun (dx,dy) ->
                let nx,ny = u.X+dx,u.Y+dy
                if grid.[ny,nx]='.' && stepD.ContainsKey (nx,ny) then Some (stepD.[(nx,ny)],ny,nx) else None)
            |> Array.sort
        if bestStep.Length>0 then
            let _,ny,nx = bestStep.[0]
            grid.[u.Y,u.X] <- '.'
            u.X <- nx; u.Y <- ny
            grid.[u.Y,u.X] <- u.Type

let attack (u:Unit) (targets:Unit list) (grid:char[,]) =
    let adj =
        targets
        |> List.filter (fun t -> abs(t.X-u.X)+abs(t.Y-u.Y)=1)
        |> List.sortBy (fun t -> t.Hp,t.Y,t.X)
        |> List.tryHead
    match adj with
    | None -> ()
    | Some t ->
        t.Hp <- t.Hp - u.Atk
        if t.Dead then grid.[t.Y,t.X] <- '.'

[<EntryPoint>]
let main _ =
    let grid,units = readInput()
    let mutable rounds = 0
    let mutable go = true
    while go do
        units.Sort(fun a b -> compare (a.Y,a.X) (b.Y,b.X))
        let mutable fullRound = true
        for i in 0..units.Count-1 do
            let u = units.[i]
            if not u.Dead then
                let targets = units |> Seq.filter (fun t -> not t.Dead && t.Type<>u.Type) |> List.ofSeq
                if targets.IsEmpty then fullRound <- false; go <- false
                elif targets |> List.exists (fun t -> abs(t.X-u.X)+abs(t.Y-u.Y)=1) |> not then
                    move u grid targets
                attack u targets grid
        if go then rounds <- rounds + 1
    let sum = units |> Seq.filter (fun u -> not u.Dead) |> Seq.sumBy (fun u -> u.Hp)
    printfn "%d" (rounds*sum)
    0
