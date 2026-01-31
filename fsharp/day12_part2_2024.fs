
open System
open System.IO

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines "input.txt"
    let H = lines.Length
    let W = lines.[0].Length
    let grid = Array2D.init (H+2) (W+2) (fun r c -> 
        if r=0 || c=0 || r=H+1 || c=W+1 then '.' else lines.[r-1].[c-1])
    let visited = Array2D.create (H+2) (W+2) false
    let mutable p1, p2 = 0L, 0L

    for r0 in 1..H do
     for c0 in 1..W do
      if not visited.[r0,c0] then
        let plant = grid.[r0,c0]
        let q = System.Collections.Generic.Queue<_>(H*W)
        q.Enqueue(r0,c0)
        visited.[r0,c0] <- true
        let mutable area, peri = 0, 0
        let cells = System.Collections.Generic.List<_>(H*W)

        while q.Count > 0 do
            let r,c = q.Dequeue()
            area <- area + 1
            cells.Add(r,c)
            for dr,dc in [|(-1,0);(1,0);(0,-1);(0,1)|] do
                let nr,nc = r+dr,c+dc
                if grid.[nr,nc] <> plant then peri <- peri + 1
                elif not visited.[nr,nc] then
                    visited.[nr,nc] <- true
                    q.Enqueue(nr,nc)

        let inline exposed r c = grid.[r,c] <> plant
        let mutable top, bot, lef, rig = 0,0,0,0
        let mutable topAdj, botAdj, lefAdj, rigAdj = 0,0,0,0
        let inReg = Array2D.create (H+2) (W+2) false
        for r,c in cells do inReg.[r,c] <- true
        for r,c in cells do
            if exposed (r-1) c then top <- top + 1
            if exposed (r+1) c then bot <- bot + 1
            if exposed r (c-1) then lef <- lef + 1
            if exposed r (c+1) then rig <- rig + 1
            if inReg.[r  ,c+1] then
                if exposed (r-1) c && exposed (r-1) (c+1) then topAdj <- topAdj + 1
                if exposed (r+1) c && exposed (r+1) (c+1) then botAdj <- botAdj + 1
            if inReg.[r+1,c  ] then
                if exposed r (c-1) && exposed (r+1) (c-1) then lefAdj <- lefAdj + 1
                if exposed r (c+1) && exposed (r+1) (c+1) then rigAdj <- rigAdj + 1

        let sides = (top - topAdj) + (bot - botAdj) + (lef - lefAdj) + (rig - rigAdj)
        p1 <- p1 + int64 area * int64 peri
        p2 <- p2 + int64 area * int64 sides

    printfn "%d" p1
    printfn "%d" p2
    0
