
open System
open System.IO
open System.Collections.Generic

type Unit = { kind: char; mutable x: int; mutable y: int; mutable hp: int; power: int; mutable alive: bool }

let getDists startX startY excludeX excludeY (units: Unit[]) (grid: char[][]) rows cols =
    let dists = Dictionary<int * int, int>()
    let q = Queue<int * int>()
    let occupied = HashSet<int * int>()
    for u in units do if u.alive then occupied.Add((u.x, u.y)) |> ignore
    occupied.Remove((excludeX, excludeY)) |> ignore
    if not (occupied.Contains(startX, startY)) then
        dists.[(startX, startY)] <- 0
        q.Enqueue((startX, startY))
    while q.Count > 0 do
        let (cx, cy) = q.Dequeue()
        let d = dists.[(cx, cy)]
        for nx, ny in [| cx, cy - 1; cx - 1, cy; cx + 1, cy; cx, cy + 1 |] do
            if nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid.[ny].[nx] = '.' && not (occupied.Contains(nx, ny)) && not (dists.ContainsKey(nx, ny)) then
                dists.[(nx, ny)] <- d + 1
                q.Enqueue((nx, ny))
    dists

let simulate lines rows cols elfPower part2 =
    let units = List<Unit>()
    let grid = lines |> Array.map (fun (s: string) -> s.ToCharArray())
    for y in 0 .. rows - 1 do
        for x in 0 .. cols - 1 do
            if grid.[y].[x] = 'E' || grid.[y].[x] = 'G' then
                units.Add({ kind = grid.[y].[x]; x = x; y = y; hp = 200; power = (if grid.[y].[x] = 'E' then elfPower else 3); alive = true })
                grid.[y].[x] <- '.'
    let unitsArr = units.ToArray()
    let mutable rounds = 0
    let mutable combatOver = false
    let mutable elfDied = false
    while not combatOver && not elfDied do
        let sorted = unitsArr |> Array.filter (fun u -> u.alive) |> Array.sortBy (fun u -> u.y, u.x)
        for u in sorted do
            if u.alive && not combatOver && not elfDied then
                let enemies = unitsArr |> Array.filter (fun e -> e.alive && e.kind <> u.kind)
                if enemies.Length = 0 then combatOver <- true
                else
                    let isAdj (e: Unit) = abs (u.x - e.x) + abs (u.y - e.y) = 1
                    if not (enemies |> Array.exists isAdj) then
                        let dists = getDists u.x u.y u.x u.y unitsArr grid rows cols
                        let reachableTargets = 
                            enemies |> Seq.collect (fun e -> [| e.x, e.y - 1; e.x - 1, e.y; e.x + 1, e.y; e.x, e.y + 1 |])
                            |> Seq.distinct |> Seq.filter (fun n -> dists.ContainsKey n) |> Seq.toList
                        if not reachableTargets.IsEmpty then
                            let minDist = reachableTargets |> List.map (fun n -> dists.[n]) |> List.min
                            let target = reachableTargets |> List.filter (fun n -> dists.[n] = minDist) |> List.minBy (fun (nx, ny) -> ny, nx)
                            let moveDists = getDists (fst target) (snd target) u.x u.y unitsArr grid rows cols
                            let nextStep = [| u.x, u.y - 1; u.x - 1, u.y; u.x + 1, u.y; u.x, u.y + 1 |]
                                           |> Array.filter (fun n -> moveDists.ContainsKey n)
                                           |> Array.minBy (fun n -> moveDists.[n], snd n, fst n)
                            u.x <- fst nextStep; u.y <- snd nextStep
                    let adjEnemies = enemies |> Array.filter (fun e -> abs (u.x - e.x) + abs (u.y - e.y) = 1)
                    if adjEnemies.Length > 0 then
                        let target = adjEnemies |> Array.minBy (fun e -> e.hp, e.y, e.x)
                        target.hp <- target.hp - u.power
                        if target.hp <= 0 then
                            target.alive <- false
                            if part2 && target.kind = 'E' then elfDied <- true
        if not combatOver && not elfDied then rounds <- rounds + 1
    if elfDied then None else Some (rounds * (unitsArr |> Array.filter (fun u -> u.alive) |> Array.sumBy (fun u -> u.hp)))

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt" |> Array.filter (not << String.IsNullOrWhiteSpace)
    let rows, cols = lines.Length, lines.[0].Length
    let mutable p, found, res = 4, false, 0
    while not found do
        match simulate lines rows cols p true with
        | Some r -> res <- r; found <- true
        | None -> p <- p + 1
    printfn "%d" res
    0
