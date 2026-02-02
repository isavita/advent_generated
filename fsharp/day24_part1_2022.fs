
open System
open System.Collections.Generic
open System.IO

type Point = { x: int; y: int }

let dx = [| 0; 0; 1; -1; 0 |]
let dy = [| 1; -1; 0; 0; 0 |]
let bliz = [| '^'; '>'; 'v'; '<' |]

let steps (grid: char[][]) rows cols (start: Point) (end_: Point) initialStep =
    let q = new Queue<Point * int>()
    q.Enqueue((start, initialStep))
    let seen = Array3D.create rows cols (rows * cols) false
    seen.[start.y, start.x, initialStep % (rows * cols)] <- true

    let rec loop () =
        if q.Count = 0 then -1
        else
            let (pos, step) = q.Dequeue()
            if pos.x = end_.x && pos.y = end_.y then step
            else
                for i = 0 to 4 do
                    let nx = pos.x + dx.[i]
                    let ny = pos.y + dy.[i]
                    let ns = step + 1
                    if nx >= 0 && nx < cols && ny >= 0 && ny < rows && grid.[ny].[nx] <> '#' && not seen.[ny, nx, ns % (rows * cols)] then
                        let valid =
                            if ny > 0 && ny < rows - 1 then
                                let check j =
                                    let px, py =
                                        match j with
                                        | 0 ->
                                            let py = (ny + ns) % (rows - 2)
                                            let py = if py = 0 then rows - 2 else py
                                            (nx, py)
                                        | 1 ->
                                            let px = (nx - ns) % (cols - 2)
                                            let px = if px < 0 then px + cols - 2 else px
                                            let px = if px = 0 then cols - 2 else px
                                            (px, ny)
                                        | 2 ->
                                            let py = (ny - ns) % (rows - 2)
                                            let py = if py < 0 then py + rows - 2 else py
                                            let py = if py = 0 then rows - 2 else py
                                            (nx, py)
                                        | _ ->
                                            let px = (nx + ns) % (cols - 2)
                                            let px = if px = 0 then cols - 2 else px
                                            (px, ny)
                                    grid.[py].[px] = bliz.[j]
                                not (Array.exists check [|0..3|])
                            else true
                        if valid then
                            seen.[ny, nx, ns % (rows * cols)] <- true
                            q.Enqueue(({ x = nx; y = ny }, ns))
                loop ()

    loop ()

let main () =
    let lines = File.ReadAllLines("input.txt")
    let rows = lines.Length
    let cols = lines.[0].Length
    let grid = Array.init rows (fun i -> lines.[i].ToCharArray())
    let entrance = { x = 1; y = 0 }
    let exit = { x = cols - 2; y = rows - 1 }
    printfn "%d" (steps grid rows cols entrance exit 0)

main ()
