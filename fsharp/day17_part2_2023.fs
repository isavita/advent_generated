
open System
open System.Collections.Generic
open System.IO

type Coord = { x: int; y: int }
    with
        static member (+) (a: Coord, b: Coord) = { x = a.x + b.x; y = a.y + b.y }
        static member (-) (a: Coord, b: Coord) = { x = a.x - b.x; y = a.y - b.y }
        member this.Opposite = { x = -this.x; y = -this.y }

type Info = { coord: Coord; dir: Coord; numStraight: int }

type Grid = { width: int; height: int; data: Map<Coord, int> }
    with
        member this.Neighbors4(c: Coord) =
            let neighbors = 
                [ { x = c.x; y = c.y - 1 }; { x = c.x - 1; y = c.y }; { x = c.x; y = c.y + 1 }; { x = c.x + 1; y = c.y } ]
                |> List.filter (fun n -> n.x >= 0 && n.x < this.width && n.y >= 0 && n.y < this.height)
            neighbors

type MinHeap() =
    let heap = new ResizeArray<_>()
    member _.Any = heap.Count > 0
    member _.Enqueue(info: Info, priority: int) =
        heap.Add((info, priority))
        let rec bubbleUp i =
            if i = 0 then ()
            else
                let p = (i - 1) / 2
                if snd heap.[p] <= snd heap.[i] then ()
                else
                    let tmp = heap.[i]
                    heap.[i] <- heap.[p]
                    heap.[p] <- tmp
                    bubbleUp p
        bubbleUp (heap.Count - 1)
    member _.Dequeue() =
        let res = fst heap.[0]
        let last = heap.[heap.Count - 1]
        heap.[0] <- last
        heap.RemoveAt(heap.Count - 1)
        let rec bubbleDown i =
            let l = 2 * i + 1
            let r = l + 1
            let smallest =
                if l >= heap.Count then i
                elif r >= heap.Count then
                    if snd heap.[l] < snd heap.[i] then l else i
                else
                    if snd heap.[l] < snd heap.[r] then
                        if snd heap.[l] < snd heap.[i] then l else i
                    else
                        if snd heap.[r] < snd heap.[i] then r else i
            if smallest = i then ()
            else
                let tmp = heap.[i]
                heap.[i] <- heap.[smallest]
                heap.[smallest] <- tmp
                bubbleDown smallest
        bubbleDown 0
        res

let heuristic (a: Coord) (b: Coord) =
    abs (a.x - b.x) + abs (a.y - b.y)

let aStarConstrained (grid: Grid) (start: Coord) (goal: Coord) minStraight maxStraight =
    let frontier = MinHeap()
    let startInfo = { coord = start; dir = { x = 0; y = 0 }; numStraight = 0 }
    frontier.Enqueue(startInfo, 0)
    let cameFrom = Dictionary<_, _>()
    let costSoFar = Dictionary<_, _>()
    cameFrom.[startInfo] <- startInfo
    costSoFar.[startInfo] <- 0

    let rec loop() =
        if not frontier.Any then -1
        else
            let current = frontier.Dequeue()
            let currentCost = costSoFar.[current]

            if current.coord = goal then currentCost
            else
                let neighbors = grid.Neighbors4(current.coord)
                let newInfos =
                    neighbors
                    |> List.map (fun n ->
                        let newDir = n - current.coord
                        let newNumStraight = if newDir = current.dir then current.numStraight + 1 else 1
                        let nextInfo = { coord = n; dir = newDir; numStraight = newNumStraight }
                        let newCost = currentCost + grid.data.[n]
                        let isLowerCost = not (costSoFar.ContainsKey(nextInfo)) || newCost < costSoFar.[nextInfo]
                        let isValidStraight = (current.numStraight >= minStraight || newDir = current.dir || current.coord = start) && (newNumStraight <= maxStraight)
                        let isNotOppositeDirection = newDir <> current.dir.Opposite
                        (nextInfo, newCost, isLowerCost && isValidStraight && isNotOppositeDirection))
                for (nextInfo, newCost, valid) in newInfos do
                    if valid then
                        costSoFar.[nextInfo] <- newCost
                        cameFrom.[nextInfo] <- current
                        let priority = newCost + heuristic nextInfo.coord goal
                        frontier.Enqueue(nextInfo, priority)
                loop()
    loop()

let solveFromLines (input: string list) =
    let height = input.Length
    let width = input.[0].Length
    let data =
        [ for y in 0 .. height - 1 do
            for x in 0 .. width - 1 ->
                ({ x = x; y = y }, int (input.[y].[x] - '0')) ]
        |> Map.ofList
    let grid = { width = width; height = height; data = data }
    let start = { x = 0; y = 0 }
    let goal = { x = width - 1; y = height - 1 }
    aStarConstrained grid start goal 4 10

let main() =
    let input =
        File.ReadAllLines("input.txt")
        |> Seq.filter (fun l -> not (String.IsNullOrWhiteSpace l))
        |> List.ofSeq
    let ans = solveFromLines input
    printfn "%d" ans

main()
