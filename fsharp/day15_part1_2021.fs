
open System
open System.Collections.Generic
open System.IO

type Node = { x: int; y: int; risk: int }

type MinHeap() =
    let data = new ResizeArray<Node>()
    member _.Count = data.Count

    member _.Push(n: Node) =
        data.Add(n)
        let rec bubbleUp i =
            if i > 0 then
                let p = (i - 1) / 2
                if data.[i].risk >= data.[p].risk then ()
                else
                    let t = data.[i]
                    data.[i] <- data.[p]
                    data.[p] <- t
                    bubbleUp p
        bubbleUp (data.Count - 1)

    member _.Pop() =
        let min = data.[0]
        let last = data.[data.Count - 1]
        data.RemoveAt(data.Count - 1)
        if data.Count > 0 then
            data.[0] <- last
            let rec bubbleDown i =
                let left = i * 2 + 1
                let right = left + 1
                let mutable smallest = i
                if left < data.Count && data.[left].risk < data.[smallest].risk then smallest <- left
                if right < data.Count && data.[right].risk < data.[smallest].risk then smallest <- right
                if smallest = i then ()
                else
                    let t = data.[i]
                    data.[i] <- data.[smallest]
                    data.[smallest] <- t
                    bubbleDown smallest
            bubbleDown 0
        min

let dijkstra (grid: int[,]) =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let dist = Array2D.init rows cols (fun _ _ -> Int32.MaxValue)
    dist.[0, 0] <- 0

    let heap = MinHeap()
    heap.Push { x = 0; y = 0; risk = 0 }

    let dx = [| 1; 0; -1; 0 |]
    let dy = [| 0; 1; 0; -1 |]

    let rec search() =
        if heap.Count = 0 then -1
        else
            let cur = heap.Pop()
            if cur.risk <> dist.[cur.x, cur.y] then search()
            else if cur.x = rows - 1 && cur.y = cols - 1 then cur.risk
            else
                for dir = 0 to 3 do
                    let nx = cur.x + dx.[dir]
                    let ny = cur.y + dy.[dir]
                    if nx >= 0 && ny >= 0 && nx < rows && ny < cols then
                        let nextRisk = cur.risk + grid.[nx, ny]
                        if nextRisk < dist.[nx, ny] then
                            dist.[nx, ny] <- nextRisk
                            heap.Push { x = nx; y = ny; risk = nextRisk }
                search()

    search()

[<EntryPoint>]
let main argv =
    try
        let lines = File.ReadAllLines "input.txt"
        let grid =
            lines
            |> Array.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> List.ofSeq)
            |> List.ofArray
        let rows = grid.Length
        let cols = grid.[0].Length
        let grid2d = Array2D.init rows cols (fun i j -> grid.[i].[j])
        printfn "%d" (dijkstra grid2d)
        0
    with
    | :? FileNotFoundException -> printfn "Error opening file"; 1
    | ex -> printfn "An error occurred: %s" ex.Message; 1
