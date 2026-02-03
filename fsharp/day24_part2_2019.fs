
open System
open System.Collections.Generic
open System.IO

let Side = 5
let Square = Side * Side

let parse () =
    let lines = File.ReadAllLines "input.txt"
    let arr = Array.zeroCreate<bool> Square
    for r = 0 to Side - 1 do
        for c = 0 to Side - 1 do
            arr.[r * Side + c] <- lines.[r].[c] = '#'
    arr

let infested (space:Dictionary<int,bool[]>) level cell =
    match space.TryGetValue(level) with
    | true, a -> a.[cell]
    | _ -> false

let minMaxLevel (space:Dictionary<int,bool[]>) =
    let mutable mn = Int32.MaxValue
    let mutable mx = Int32.MinValue
    for k in space.Keys do
        if k < mn then mn <- k
        if k > mx then mx <- k
    mn, mx

let clean (space:Dictionary<int,bool[]>) =
    let mn, mx = minMaxLevel space
    let mutable cntMn = 0
    let mutable cntMx = 0
    let aMn = space.[mn]
    let aMx = space.[mx]
    for i = 0 to Square - 1 do
        if aMn.[i] then cntMn <- cntMn + 1
        if aMx.[i] then cntMx <- cntMx + 1
    if cntMn = 0 then space.Remove(mn) |> ignore
    if cntMx = 0 then space.Remove(mx) |> ignore

let next2 (space:Dictionary<int,bool[]>) =
    let newSpace = Dictionary<int,bool[]>()
    let mn, mx = minMaxLevel space
    for lvl = mn - 1 to mx + 1 do
        let arr = Array.zeroCreate<bool> Square
        newSpace.[lvl] <- arr
        for cell = 0 to Square - 1 do
            if cell <> 12 then
                let row = cell / Side
                let col = cell % Side
                let mutable n = 0
                if row = 0 && infested space (lvl - 1) 7 then n <- n + 1
                if col = 0 && infested space (lvl - 1) 11 then n <- n + 1
                if col = 4 && infested space (lvl - 1) 13 then n <- n + 1
                if row = 4 && infested space (lvl - 1) 17 then n <- n + 1
                if cell = 7 then for i = 0 to Side - 1 do if infested space (lvl + 1) i then n <- n + 1
                if cell = 11 then for i = 0 to Side - 1 do if infested space (lvl + 1) (5 * i) then n <- n + 1
                if cell = 13 then for i = 0 to Side - 1 do if infested space (lvl + 1) (5 * i + Side - 1) then n <- n + 1
                if cell = 17 then for i = 0 to Side - 1 do if infested space (lvl + 1) ((Side - 1) * Side + i) then n <- n + 1
                if row > 0 && cell <> 17 && infested space lvl (cell - Side) then n <- n + 1
                if col > 0 && cell <> 13 && infested space lvl (cell - 1) then n <- n + 1
                if col < Side - 1 && cell <> 11 && infested space lvl (cell + 1) then n <- n + 1
                if row < Side - 1 && cell <> 7 && infested space lvl (cell + Side) then n <- n + 1
                if infested space lvl cell then arr.[cell] <- n = 1
                else arr.[cell] <- n = 1 || n = 2
    clean newSpace
    newSpace

[<EntryPoint>]
let main argv =
    let init = parse ()
    let mutable space = Dictionary<int,bool[]>()
    space.[0] <- init
    for _ = 1 to 200 do
        space <- next2 space
    let mutable total = 0
    for grid in space.Values do
        for b in grid do
            if b then total <- total + 1
    printfn "%d" total
    0
