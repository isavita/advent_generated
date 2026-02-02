
open System
open System.Collections.Generic
open System.IO

type Point = { x: int; y: int }

type State = { pos: Point; keys: int }

let dirs = [| { x = 0; y = -1 }; { x = -1; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 0 } |]

let hashState (s: State) =
    let hash = ref 5381
    hash := (!hash <<< 5) + !hash + s.pos.x
    hash := (!hash <<< 5) + !hash + s.pos.y
    hash := (!hash <<< 5) + !hash + s.keys
    !hash

let findShortestPath (grid: char[,]) (start: Point) (keyMap: int array) (numKeys: int) =
    let width = Array2D.length1 grid
    let height = Array2D.length2 grid
    let targetKeys = (1 <<< numKeys) - 1

    let visited = new HashSet<int * int * int>()
    let queue = new Queue<State>()
    queue.Enqueue({ pos = start; keys = 0 })
    visited.Add((start.x, start.y, 0)) |> ignore

    let mutable steps = 0

    while queue.Count > 0 do
        let levelSize = queue.Count
        for _ in 1 .. levelSize do
            let current = queue.Dequeue()

            if current.keys = targetKeys then
                printfn "%d" steps
                exit 0

            for dir in dirs do
                let nextPos = { x = current.pos.x + dir.x; y = current.pos.y + dir.y }

                if nextPos.x >= 0 && nextPos.x < width && nextPos.y >= 0 && nextPos.y < height then
                    let cell = grid.[nextPos.x, nextPos.y]
                    let nextKeys =
                        match cell with
                        | c when Char.IsLower c ->
                            let keyIndex = keyMap.[int c - int 'a']
                            if keyIndex <> -1 then current.keys ||| (1 <<< keyIndex) else current.keys
                        | _ -> current.keys

                    if cell <> '#' && (not (Char.IsUpper cell) || (let keyIndex = keyMap.[int (Char.ToLower cell) - int 'a'] in keyIndex <> -1 && (current.keys &&& (1 <<< keyIndex)) <> 0)) then
                        let nextState = { pos = nextPos; keys = nextKeys }
                        let stateHash = (nextPos.x, nextPos.y, nextKeys)
                        if not (visited.Contains stateHash) then
                            visited.Add stateHash |> ignore
                            queue.Enqueue nextState

        steps <- steps + 1

    printfn "-1"

let main() =
    try
        let lines = File.ReadAllLines("input.txt")
        let height = lines.Length
        let width = lines.[0].Length
        let grid = Array2D.init width height (fun x y -> lines.[y].[x])

        let mutable start = { x = -1; y = -1 }
        let keyMap = Array.init 26 (fun _ -> -1)
        let mutable keyCounter = 0

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                match grid.[x, y] with
                | '@' ->
                    start <- { x = x; y = y }
                    grid.[x, y] <- '.'
                | c when Char.IsLower c ->
                    let keyIndex = int c - int 'a'
                    if keyMap.[keyIndex] = -1 then
                        keyMap.[keyIndex] <- keyCounter
                        keyCounter <- keyCounter + 1
                | _ -> ()

        if start.x = -1 then
            failwith "Start position '@' not found."

        if keyCounter = 0 then
            printfn "0"
        else
            findShortestPath grid start keyMap keyCounter

    with
    | ex -> printfn "%s" ex.Message

main()
