
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let grid = lines |> Array.map (fun s -> s.ToCharArray())
    let visited = HashSet<(int*int*int*int)>()
    let todo = Stack<(int*int*int*int)>()
    todo.Push((0,0,1,0))
    let energized = HashSet<(int*int)>()
    while todo.Count > 0 do
        let (x,y,dx,dy) = todo.Pop()
        if x < 0 || y < 0 || y >= grid.Length || x >= grid.[y].Length then ()
        else
            let key = (x,y,dx,dy)
            if visited.Add key then
                energized.Add((x,y)) |> ignore
                match grid.[y].[x] with
                | '.' -> todo.Push((x+dx, y+dy, dx, dy))
                | '/' -> todo.Push((x-dy, y-dx, -dy, -dx))
                | '\\' -> todo.Push((x+dy, y+dx, dy, dx))
                | '|' ->
                    if dx <> 0 then
                        todo.Push((x, y+1, 0, 1))
                        todo.Push((x, y-1, 0, -1))
                    else
                        todo.Push((x+dx, y+dy, dx, dy))
                | '-' ->
                    if dy <> 0 then
                        todo.Push((x+1, y, 1, 0))
                        todo.Push((x-1, y, -1, 0))
                    else
                        todo.Push((x+dx, y+dy, dx, dy))
                | _ -> ()
    printfn "%d" energized.Count
    0
