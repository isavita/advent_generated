
open System
open System.IO

type Hailstone = { px: int64; py: int64; pz: int64; vx: int64; vy: int64; vz: int64 }

let parseInput (filename: string) =
    File.ReadAllLines(filename)
    |> Seq.map (fun line ->
        let parts = line.Split([| '@'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        let p = Array.map int64 [| parts.[0]; parts.[1]; parts.[2] |]
        let v = Array.map int64 [| parts.[3]; parts.[4]; parts.[5] |]
        { px = p.[0]; py = p.[1]; pz = p.[2]; vx = v.[0]; vy = v.[1]; vz = v.[2] })
    |> Seq.toArray

let solvePart1 (hailstones: Hailstone[]) =
    let minCoord = 200000000000000L
    let maxCoord = 400000000000000L
    let count =
        seq {
            for i in 0 .. hailstones.Length - 2 do
                for j in i + 1 .. hailstones.Length - 1 do
                    let h1 = hailstones.[i]
                    let h2 = hailstones.[j]
                    let det = h1.vx * h2.vy - h1.vy * h2.vx
                    if det <> 0L then
                        let t1 = decimal ((h2.px - h1.px) * h2.vy - (h2.py - h1.py) * h2.vx) / decimal det
                        let t2 = decimal ((h2.px - h1.px) * h1.vy - (h2.py - h1.py) * h1.vx) / decimal det
                        if t1 > 0M && t2 > 0M then
                            let ix = decimal h1.px + decimal h1.vx * t1
                            let iy = decimal h1.py + decimal h1.vy * t1
                            if ix >= decimal minCoord && ix <= decimal maxCoord &&
                               iy >= decimal minCoord && iy <= decimal maxCoord then
                                yield 1L
        }
        |> Seq.sum
    printfn "Part 1: %d" count

let gaussianElimination (matrix: decimal[,]) =
    let n = Array2D.length1 matrix
    for i in 0 .. n - 1 do
        let mutable maxRow = i
        for k in i + 1 .. n - 1 do
            if abs matrix.[k, i] > abs matrix.[maxRow, i] then
                maxRow <- k
        for k in i .. n do
            let temp = matrix.[i, k]
            matrix.[i, k] <- matrix.[maxRow, k]
            matrix.[maxRow, k] <- temp
        for k in i + 1 .. n - 1 do
            let factor = matrix.[k, i] / matrix.[i, i]
            for j in i .. n do
                matrix.[k, j] <- matrix.[k, j] - factor * matrix.[i, j]
    for i in n - 1 .. -1 .. 0 do
        for j in i + 1 .. n - 1 do
            matrix.[i, n] <- matrix.[i, n] - matrix.[i, j] * matrix.[j, n]
        matrix.[i, n] <- matrix.[i, n] / matrix.[i, i]

let solvePart2 (hailstones: Hailstone[]) =
    if hailstones.Length < 3 then
        printfn "Part 2: Not enough data points."
    else
        let h0 = hailstones.[0]
        let h1 = hailstones.[1]
        let h2 = hailstones.[2]
        let dvx1 = h0.vx - h1.vx
        let dvy1 = h0.vy - h1.vy
        let dvz1 = h0.vz - h1.vz
        let dpx1 = h0.px - h1.px
        let dpy1 = h0.py - h1.py
        let dpz1 = h0.pz - h1.pz
        let dvx2 = h0.vx - h2.vx
        let dvy2 = h0.vy - h2.vy
        let dvz2 = h0.vz - h2.vz
        let dpx2 = h0.px - h2.px
        let dpy2 = h0.py - h2.py
        let dpz2 = h0.pz - h2.pz

        let matrix = Array2D.init 6 7 (fun i j -> 0M)
        matrix.[0, 1] <- decimal dvz1
        matrix.[0, 2] <- -decimal dvy1
        matrix.[0, 4] <- decimal dpz1
        matrix.[0, 5] <- -decimal dpy1
        matrix.[1, 0] <- -decimal dvz1
        matrix.[1, 2] <- decimal dvx1
        matrix.[1, 3] <- -decimal dpz1
        matrix.[1, 5] <- decimal dpx1
        matrix.[2, 0] <- decimal dvy1
        matrix.[2, 1] <- -decimal dvx1
        matrix.[2, 3] <- decimal dpy1
        matrix.[2, 4] <- -decimal dpx1
        matrix.[3, 1] <- decimal dvz2
        matrix.[3, 2] <- -decimal dvy2
        matrix.[3, 4] <- decimal dpz2
        matrix.[3, 5] <- -decimal dpy2
        matrix.[4, 0] <- -decimal dvz2
        matrix.[4, 2] <- decimal dvx2
        matrix.[4, 3] <- -decimal dpz2
        matrix.[4, 5] <- decimal dpx2
        matrix.[5, 0] <- decimal dvy2
        matrix.[5, 1] <- -decimal dvx2
        matrix.[5, 3] <- decimal dpy2
        matrix.[5, 4] <- -decimal dpx2

        matrix.[0, 6] <- decimal (h0.py * h0.vz - h0.pz * h0.vy) - decimal (h1.py * h1.vz - h1.pz * h1.vy)
        matrix.[1, 6] <- decimal (h0.pz * h0.vx - h0.px * h0.vz) - decimal (h1.pz * h1.vx - h1.px * h1.vz)
        matrix.[2, 6] <- decimal (h0.px * h0.vy - h0.py * h0.vx) - decimal (h1.px * h1.vy - h1.py * h1.vx)
        matrix.[3, 6] <- decimal (h0.py * h0.vz - h0.pz * h0.vy) - decimal (h2.py * h2.vz - h2.pz * h2.vy)
        matrix.[4, 6] <- decimal (h0.pz * h0.vx - h0.px * h0.vz) - decimal (h2.pz * h2.vx - h2.px * h2.vz)
        matrix.[5, 6] <- decimal (h0.px * h0.vy - h0.py * h0.vx) - decimal (h2.px * h2.vy - h2.py * h2.vx)

        gaussianElimination matrix

        let prx = int64 (round matrix.[0, 6])
        let pry = int64 (round matrix.[1, 6])
        let prz = int64 (round matrix.[2, 6])
        printfn "Part 2: %d" (prx + pry + prz)

let main () =
    let hailstones = parseInput "input.txt"
    solvePart1 hailstones
    solvePart2 hailstones

main ()
