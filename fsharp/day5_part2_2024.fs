
open System
open System.IO
open System.Collections.Generic

type Rule = {x:int; y:int}

let isCorrect (update:int[]) (rules:Rule[]) =
    let pos = Dictionary<int,int>()
    update |> Array.iteri (fun i p -> pos.[p] <- i)
    let mutable ok = true
    for r in rules do
        let hasX, posX = pos.TryGetValue r.x
        let hasY, posY = pos.TryGetValue r.y
        if hasX && hasY && posX > posY then
            ok <- false
    ok

let correctOrder (update:int[]) (rules:Rule[]) =
    let indeg = Dictionary<int,int>()
    let adj = Dictionary<int,ResizeArray<int>>()
    for p in update do
        indeg.[p] <- 0
        adj.[p] <- ResizeArray<int>()
    for r in rules do
        let hasX = indeg.ContainsKey r.x
        let hasY = indeg.ContainsKey r.y
        if hasX && hasY then
            adj.[r.x].Add(r.y)
            indeg.[r.y] <- indeg.[r.y] + 1
    let q = Queue<int>()
    for kv in indeg do
        if kv.Value = 0 then q.Enqueue(kv.Key)
    let sorted = ResizeArray<int>()
    while q.Count > 0 do
        let u = q.Dequeue()
        sorted.Add(u)
        for v in adj.[u] do
            indeg.[v] <- indeg.[v] - 1
            if indeg.[v] = 0 then q.Enqueue(v)
    sorted.ToArray()

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines("input.txt")
    let rules = ResizeArray<Rule>()
    let updates = ResizeArray<int[]>()
    let mutable readingRules = true
    for line in lines do
        let l = line.Trim()
        if l <> "" then
            if l.Contains("|") then
                if readingRules then
                    let parts = l.Split('|')
                    if parts.Length = 2 then
                        let x = int parts.[0]
                        let y = int parts.[1]
                        rules.Add({x=x; y=y})
            else
                readingRules <- false
                let pages = l.Split([|','|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int
                if pages.Length > 0 then updates.Add(pages)
    let rulesArr = rules.ToArray()
    let mutable sum = 0L
    for upd in updates do
        if not (isCorrect upd rulesArr) then
            let order = correctOrder upd rulesArr
            if order.Length > 0 then
                sum <- sum + int64 order.[order.Length / 2]
    printfn "%d" sum
    0
