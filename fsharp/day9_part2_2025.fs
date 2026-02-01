open System
open System.IO
open System.Collections.Generic

type Point = { X:int; Y:int }

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    let pts = ResizeArray<Point>()
    let xsList = ResizeArray<int>()
    let ysList = ResizeArray<int>()
    for line in lines do
        let s = line.Trim()
        if s.Length > 0 then
            let parts = s.Split(',')
            if parts.Length = 2 then
                match Int32.TryParse(parts.[0].Trim()), Int32.TryParse(parts.[1].Trim()) with
                | (true,x),(true,y) ->
                    pts.Add({X=x;Y=y})
                    xsList.Add(x)
                    ysList.Add(y)
                | _ -> ()
    if pts.Count = 0 then
        printfn "Largest valid area: 0"
        0
    else
        xsList.Sort()
        ysList.Sort()
        let xs = ResizeArray<int>()
        let ys = ResizeArray<int>()
        let mutable prev = Int32.MinValue
        for v in xsList do
            if v <> prev then xs.Add(v); prev <- v
        prev <- Int32.MinValue
        for v in ysList do
            if v <> prev then ys.Add(v); prev <- v
        let xidx = Dictionary<int,int>()
        for i = 0 to xs.Count-1 do xidx.[xs.[i]] <- i
        let yidx = Dictionary<int,int>()
        for i = 0 to ys.Count-1 do yidx.[ys.[i]] <- i
        let W = 2*xs.Count + 1
        let H = 2*ys.Count + 1
        let colW = Array.zeroCreate<int64> W
        let rowH = Array.zeroCreate<int64> H
        colW.[0] <- 1L
        for i = 0 to xs.Count-1 do
            colW.[2*i+1] <- 1L
            colW.[2*i+2] <- if i+1 < xs.Count then int64 (max 0 (xs.[i+1]-xs.[i]-1)) else 1L
        rowH.[0] <- 1L
        for i = 0 to ys.Count-1 do
            rowH.[2*i+1] <- 1L
            rowH.[2*i+2] <- if i+1 < ys.Count then int64 (max 0 (ys.[i+1]-ys.[i]-1)) else 1L
        let grid = Array2D.create H W 0uy
        for i = 0 to pts.Count-1 do
            let a = pts.[i]
            let b = pts.[(i+1) % pts.Count]
            let gx1 = 2*xidx.[a.X] + 1
            let gy1 = 2*yidx.[a.Y] + 1
            let gx2 = 2*xidx.[b.X] + 1
            let gy2 = 2*yidx.[b.Y] + 1
            if gx1 = gx2 then
                let y0 = min gy1 gy2
                let y1 = max gy1 gy2
                for y = y0 to y1 do
                    if rowH.[y] > 0L then grid.[y,gx1] <- 1uy
            else
                let x0 = min gx1 gx2
                let x1 = max gx1 gx2
                for x = x0 to x1 do
                    if colW.[x] > 0L then grid.[gy1,x] <- 1uy
        let q = Array.zeroCreate<Point> (W*H)
        let mutable qh = 0
        let mutable qt = 0
        q.[qt] <- {X=0;Y=0}
        qt <- qt+1
        grid.[0,0] <- 2uy
        let dx = [|0;0;1;-1|]
        let dy = [|1;-1;0;0|]
        while qh < qt do
            let cur = q.[qh]
            qh <- qh+1
            for d = 0 to 3 do
                let nx = cur.X + dx.[d]
                let ny = cur.Y + dy.[d]
                if nx >= 0 && nx < W && ny >= 0 && ny < H && grid.[ny,nx] = 0uy then
                    grid.[ny,nx] <- 2uy
                    q.[qt] <- {X=nx;Y=ny}
                    qt <- qt+1
        let P = Array2D.create H W 0L
        for y = 0 to H-1 do
            for x = 0 to W-1 do
                let v = if grid.[y,x] <> 2uy then colW.[x]*rowH.[y] else 0L
                let left = if x>0 then P.[y,x-1] else 0L
                let up = if y>0 then P.[y-1,x] else 0L
                let diag = if x>0 && y>0 then P.[y-1,x-1] else 0L
                P.[y,x] <- v + left + up - diag
        let mutable maxArea = 0L
        for i = 0 to pts.Count-1 do
            for j = i to pts.Count-1 do
                let a = pts.[i]
                let b = pts.[j]
                let w = int64 (abs (a.X - b.X) + 1)
                let h = int64 (abs (a.Y - b.Y) + 1)
                let area = w*h
                if area > maxArea then
                    let mutable gx1 = 2*xidx.[a.X] + 1
                    let mutable gy1 = 2*yidx.[a.Y] + 1
                    let mutable gx2 = 2*xidx.[b.X] + 1
                    let mutable gy2 = 2*yidx.[b.Y] + 1
                    if gx1 > gx2 then let t=gx1 in gx1<-gx2; gx2<-t
                    if gy1 > gy2 then let t=gy1 in gy1<-gy2; gy2<-t
                    let total = P.[gy2,gx2]
                    let leftSum = if gx1>0 then P.[gy2,gx1-1] else 0L
                    let upSum = if gy1>0 then P.[gy1-1,gx2] else 0L
                    let diagSum = if gx1>0 && gy1>0 then P.[gy1-1,gx1-1] else 0L
                    let valid = total - leftSum - upSum + diagSum
                    if valid = area then maxArea <- area
        printfn "Largest valid area: %d" maxArea
        0