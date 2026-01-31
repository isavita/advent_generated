
open System
open System.IO
open System.Text.RegularExpressions

let OFFSET = 10000000000000L

let parse (line: string) =
    let m = Regex.Matches(line, @"[+-]?\d+")
    (int64 m.[0].Value, int64 m.[1].Value)

let solve (ax, ay) (bx, by) (px, py) =
    let px, py = px + OFFSET, py + OFFSET
    let d  = ax * by - ay * bx
    let da = px * by - py * bx
    let db = py * ax - px * ay
    if d = 0L || da % d <> 0L || db % d <> 0L then None
    else
        let a, b = da / d, db / d
        if a < 0L || b < 0L then None
        else Some (3L * a + b)

let blocks =
    File.ReadAllText("input.txt").Split([|"\r\n\r\n"; "\n\n"|], StringSplitOptions.RemoveEmptyEntries)

let costs =
    blocks
    |> Array.choose (fun block ->
        let lines = block.Split('\n')
        let ax, ay = parse lines.[0]
        let bx, by = parse lines.[1]
        let px, py = parse lines.[2]
        solve (ax, ay) (bx, by) (px, py))

printfn "%d %d" costs.Length (Array.sum costs)
