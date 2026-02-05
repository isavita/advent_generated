open System
open System.IO
open System.Collections.Generic

type Elf(x:int, y:int) =
    member val X = x with get, set
    member val Y = y with get, set
    member val Moving = false with get, set
    member val NextX = 0 with get, set
    member val NextY = 0 with get, set

let dirs = [| [| -1; -1 |]; [| -1; 0 |]; [| -1; 1 |]; [| 0; 1 |]; [| 1; 1 |]; [| 1; 0 |]; [| 1; -1 |]; [| 0; -1 |] |]
let order = [| 1; 5; 7; 3 |]

let hash x y = x * 10000 + y

let aroundAllEmpty (e:Elf) (map: Dictionary<int,Elf>) =
    let mutable ok = true
    for d in dirs do
        let nx = e.X + d.[0]
        let ny = e.Y + d.[1]
        if map.ContainsKey(hash nx ny) then ok <- false
    ok

let elfInDirection (e:Elf) (wannaGo:int) (map: Dictionary<int,Elf>) =
    let mutable found = false
    for j in -1 .. 1 do
        let dirIndex = (wannaGo + j + 8) % 8
        let d = dirs.[dirIndex]
        if map.ContainsKey(hash (e.X + d.[0]) (e.Y + d.[1])) then
            found <- true
    found

let run (elves: ResizeArray<Elf>) (map: Dictionary<int,Elf>) (currDir:int) : bool =
    let mutable proposals = new Dictionary<int,int>()
    for elf in elves do
        if not (aroundAllEmpty elf map) then
            let mutable found = false
            for i in 0 .. 3 do
                if not found then
                    let dir_ = order.[(currDir + i) % 4]
                    if not (elfInDirection elf dir_ map) then
                        let dx = dirs.[dir_].[0]
                        let dy = dirs.[dir_].[1]
                        let destX = elf.X + dx
                        let destY = elf.Y + dy
                        let destHash = hash destX destY
                        let mutable cnt = 0
                        if proposals.TryGetValue(destHash, &cnt) then
                            proposals.[destHash] <- cnt + 1
                        else
                            proposals.Add(destHash, 1)
                        elf.NextX <- destX
                        elf.NextY <- destY
                        elf.Moving <- true
                        found <- true
    let mutable someoneMoved = false
    for elf in elves do
        if elf.Moving then
            let nextHash = hash elf.NextX elf.NextY
            let mutable c = 0
            let has = proposals.TryGetValue(nextHash, &c)
            if has && c > 1 then
                elf.Moving <- false
            else
                someoneMoved <- true
                map.Remove(hash elf.X elf.Y) |> ignore
                elf.X <- elf.NextX
                elf.Y <- elf.NextY
                map.[hash elf.X elf.Y] <- elf
                elf.Moving <- false
    someoneMoved

[<EntryPoint>]
let main argv =
    let mutable currDir = 0
    let elves = new ResizeArray<Elf>()
    let map = new Dictionary<int,Elf>()
    let lines = File.ReadAllLines("input.txt")
    for row = 0 to lines.Length - 1 do
        let line = lines.[row]
        for col = 0 to line.Length - 1 do
            if line.[col] = '#' then
                let e = new Elf(row, col)
                elves.Add(e)
                map.[hash row col] <- e

    let mutable i = 0
    let mutable finished = false
    while not finished do
        if not (run elves map currDir) then
            printfn "%d" (i + 1)
            finished <- true
        else
            currDir <- (currDir + 1) % 4
            i <- i + 1
    0