
open System
open System.IO
open System.Collections.Generic

let MAX_MATERIALS = 10
let NUM_FLOORS = 4

type Pair = { mutable g: byte; mutable c: byte }

let getMatId (name:string) (mats:ResizeArray<string>) =
    match mats.IndexOf(name) with
    | -1 ->
        mats.Add(name)
        mats.Count - 1
    | i -> i

let getKey (floors:uint32[]) (elevator:int) (numMats:int) =
    let pairs = Array.init numMats (fun _ -> { g = 0uy; c = 0uy })
    for i = 0 to numMats - 1 do
        for f = 0 to NUM_FLOORS - 1 do
            if ((floors.[f] >>> i) &&& 1u) <> 0u then pairs.[i].g <- byte f
            if ((floors.[f] >>> (i + MAX_MATERIALS)) &&& 1u) <> 0u then pairs.[i].c <- byte f
    Array.sortInPlaceBy (fun p -> (int p.g, int p.c)) pairs
    let mutable key = uint64 elevator
    for p in pairs do
        key <- (key <<< 4) ||| uint64 ((int p.g <<< 2) ||| int p.c)
    key

let isFloorValid (floor:uint32) =
    let gens = floor &&& ((1u <<< MAX_MATERIALS) - 1u)
    let chips = floor >>> MAX_MATERIALS
    gens = 0u || (chips &&& ~~~gens) = 0u

let solve () =
    let lines = File.ReadAllLines("input.txt")
    let mats = ResizeArray<string>()
    let initFloors = Array.zeroCreate<uint32> NUM_FLOORS
    for f = 0 to min (lines.Length - 1) (NUM_FLOORS - 1) do
        let mutable line = lines.[f]
        for ch in [|','; '.'; '-'|] do line <- line.Replace(string ch, " ")
        let words = line.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
        for i = 0 to words.Length - 1 do
            match words.[i] with
            | "generator" ->
                let id = getMatId words.[i-1] mats
                initFloors.[f] <- initFloors.[f] ||| (1u <<< id)
            | "microchip" ->
                let id = getMatId words.[i-2] mats
                initFloors.[f] <- initFloors.[f] ||| (1u <<< (id + MAX_MATERIALS))
            | _ -> ()
    let elId = getMatId "elerium" mats
    initFloors.[0] <- initFloors.[0] ||| (1u <<< elId) ||| (1u <<< (elId + MAX_MATERIALS))
    let diId = getMatId "dilithium" mats
    initFloors.[0] <- initFloors.[0] ||| (1u <<< diId) ||| (1u <<< (diId + MAX_MATERIALS))
    let numMats = mats.Count
    let finalMask = 
        let mutable m = 0u
        for i = 0 to numMats - 1 do
            m <- m ||| (1u <<< i) ||| (1u <<< (i + MAX_MATERIALS))
        m
    let q = Queue<(uint32[] * int * int)>()
    let visited = HashSet<uint64>()
    q.Enqueue (Array.copy initFloors, 0, 0)
    visited.Add (getKey initFloors 0 numMats) |> ignore
    let mutable result = -1
    while q.Count > 0 && result = -1 do
        let (floors, elev, steps) = q.Dequeue()
        if floors.[NUM_FLOORS-1] = finalMask then result <- steps
        else
            let items = 
                [ for i = 0 to 2*MAX_MATERIALS-1 do
                    if ((floors.[elev] >>> i) &&& 1u) <> 0u then yield i ]
            for d in [-1; 1] do
                let ne = elev + d
                if ne >= 0 && ne < NUM_FLOORS then
                    for i = 0 to items.Length-1 do
                        let move1 = 1u <<< items.[i]
                        let nxtFloors1 = Array.copy floors
                        nxtFloors1.[elev] <- nxtFloors1.[elev] &&& ~~~move1
                        nxtFloors1.[ne] <- nxtFloors1.[ne] ||| move1
                        if isFloorValid nxtFloors1.[ne] && isFloorValid nxtFloors1.[elev] then
                            let k = getKey nxtFloors1 ne numMats
                            if visited.Add k then q.Enqueue (nxtFloors1, ne, steps+1)
                        for j = i+1 to items.Length-1 do
                            let move2 = move1 ||| (1u <<< items.[j])
                            let nxtFloors2 = Array.copy floors
                            nxtFloors2.[elev] <- nxtFloors2.[elev] &&& ~~~move2
                            nxtFloors2.[ne] <- nxtFloors2.[ne] ||| move2
                            if isFloorValid nxtFloors2.[ne] && isFloorValid nxtFloors2.[elev] then
                                let k2 = getKey nxtFloors2 ne numMats
                                if visited.Add k2 then q.Enqueue (nxtFloors2, ne, steps+1)
    result

[<EntryPoint>]
let main argv =
    printfn "%d" (solve())
    0
