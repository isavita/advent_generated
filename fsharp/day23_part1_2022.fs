open System
open System.IO
open System.Collections.Generic

type Coord = int * int

[<EntryPoint>]
let main argv =
    let path = "input.txt"
    if not (File.Exists(path)) then
        printfn "0"
        0
    else
        let lines = File.ReadAllLines(path)
        let elves = HashSet<Coord>()
        for y = 0 to lines.Length - 1 do
            let line = lines.[y]
            for x = 0 to line.Length - 1 do
                if line.[x] = '#' then
                    elves.Add((x, y)) |> ignore

        let adjacent = [|(-1,-1); (0,-1); (1,-1); (-1,0); (1,0); (-1,1); (0,1); (1,1)|]
        let dirChecks : Coord[][] = [|
            [|(-1,-1); (0,-1); (1,-1)|]
            [|(-1,1); (0,1); (1,1)|]
            [|(-1,-1); (-1,0); (-1,1)|]
            [|(1,-1); (1,0); (1,1)|]
        |]
        let dirMoves: Coord[] = [| (0,-1); (0,1); (-1,0); (1,0) |]
        let mutable dirOrder = [|0; 1; 2; 3|]
        let rounds = 10

        for round = 0 to rounds - 1 do
            let proposals = Dictionary<Coord, (int * Coord)>()
            for elf in elves do
                let ex, ey = elf
                let hasNeighbor = adjacent |> Array.exists (fun (dx, dy) -> elves.Contains((ex + dx, ey + dy)))
                if not hasNeighbor then
                    ()
                else
                    let mutable idx = 0
                    let mutable proposed = false
                    while idx < 4 && not proposed do
                        let dir = dirOrder.[idx]
                        let checks3 = dirChecks.[dir]
                        let canMoveDir = checks3 |> Array.forall (fun (dx, dy) -> not (elves.Contains((ex + dx, ey + dy))))
                        if canMoveDir then
                            let (mdx, mdy) = dirMoves.[dir]
                            let dest = (ex + mdx, ey + mdy)
                            if proposals.ContainsKey(dest) then
                                let (count, firstProposer) = proposals.[dest]
                                proposals.[dest] <- (count + 1, firstProposer)
                            else
                                proposals.Add(dest, (1, elf))
                            proposed <- true
                        idx <- idx + 1
            let mutable movesMade = 0
            for dest in proposals.Keys do
                let (count, firstProposer) = proposals.[dest]
                if count = 1 then
                    if elves.Remove(firstProposer) then
                        elves.Add(dest) |> ignore
                        movesMade <- movesMade + 1
            let firstDir = dirOrder.[0]
            for i = 0 to 2 do dirOrder.[i] <- dirOrder.[i + 1]
            dirOrder.[3] <- firstDir

        if elves.Count = 0 then
            printfn "0"
            0
        else
            let mutable minX = Int32.MaxValue
            let mutable maxX = Int32.MinValue
            let mutable minY = Int32.MaxValue
            let mutable maxY = Int32.MinValue
            for (x, y) in elves do
                if x < minX then minX <- x
                if x > maxX then maxX <- x
                if y < minY then minY <- y
                if y > maxY then maxY <- y
            let width = maxX - minX + 1
            let height = maxY - minY + 1
            let empty = width * height - elves.Count
            printfn "%d" empty
            0