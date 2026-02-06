
open System
open System.IO
open System.Collections.Generic

[<Struct>]
type P = { x: int; y: int; z: int }

let rot p i =
    let x,y,z = p.x, p.y, p.z
    match i with
    | 0 -> {x=x; y=y; z=z} | 1 -> {x=x; y=z; z= -y} | 2 -> {x=x; y= -y; z= -z} | 3 -> {x=x; y= -z; z=y}
    | 4 -> {x=y; y=x; z= -z} | 5 -> {x=y; y=z; z=x} | 6 -> {x=y; y= -x; z=z} | 7 -> {x=y; y= -z; z= -x}
    | 8 -> {x=z; y=x; z=y} | 9 -> {x=z; y=y; z= -x} | 10 -> {x=z; y= -x; z= -y} | 11 -> {x=z; y= -y; z=x}
    | 12 -> {x= -x; y=y; z= -z} | 13 -> {x= -x; y=z; z=y} | 14 -> {x= -x; y= -y; z=z} | 15 -> {x= -x; y= -z; z= -y}
    | 16 -> {x= -y; y=x; z=z} | 17 -> {x= -y; y=z; z= -x} | 18 -> {x= -y; y= -x; z= -z} | 19 -> {x= -y; y= -z; z=x}
    | 20 -> {x= -z; y=x; z= -y} | 21 -> {x= -z; y=y; z=x} | 22 -> {x= -z; y= -x; z=y} | 23 -> {x= -z; y= -y; z= -x}
    | _ -> p

[<EntryPoint>]
let main _ =
    if not (File.Exists("input.txt")) then 0 else
    let content = File.ReadAllText("input.txt")
    let scanners = 
        content.Split([| "--- scanner" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun b ->
            b.Trim().Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun l -> l.Contains(","))
            |> Array.map (fun l -> 
                let c = l.Split(',') |> Array.map int
                {x=c.[0]; y=c.[1]; z=c.[2]})
            |> Array.toList)
    
    if scanners.Length = 0 then
        printfn "0"
        0
    else
        let mutable scannerPos = [{x=0; y=0; z=0}]
        let mutable queue = [scanners.[0]]
        let mutable unaligned = scanners.[1..] |> Array.toList
        
        while queue.Length > 0 do
            let current = queue.Head
            queue <- queue.Tail
            let mutable nextUnaligned = []
            for s in unaligned do
                let mutable found = false
                let sRots = s |> List.map (fun p -> [| for i in 0..23 -> rot p i |])
                for r in 0..23 do
                    if not found then
                        let rotated = sRots |> List.map (fun rots -> rots.[r])
                        let deltas = Dictionary<P, int>()
                        for rb in rotated do
                            for ab in current do
                                let d = {x=ab.x-rb.x; y=ab.y-rb.y; z=ab.z-rb.z}
                                match deltas.TryGetValue(d) with
                                | true, count -> deltas.[d] <- count + 1
                                | _ -> deltas.[d] <- 1
                        
                        let mutable delta = {x=0; y=0; z=0}
                        let mutable matchFound = false
                        for kv in deltas do
                            if kv.Value >= 12 then 
                                delta <- kv.Key
                                matchFound <- true
                        
                        if matchFound then
                            let placed = rotated |> List.map (fun p -> {x=p.x+delta.x; y=p.y+delta.y; z=p.z+delta.z})
                            scannerPos <- delta :: scannerPos
                            queue <- placed :: queue
                            found <- true
                if not found then nextUnaligned <- s :: nextUnaligned
            unaligned <- nextUnaligned

        let dist a b = abs (a.x - b.x) + abs (a.y - b.y) + abs (a.z - b.z)
        let mutable maxD = 0
        let posArr = scannerPos |> Array.ofList
        for i in 0 .. posArr.Length - 1 do
            for j in i + 1 .. posArr.Length - 1 do
                let d = dist posArr.[i] posArr.[j]
                if d > maxD then maxD <- d
        printfn "%d" maxD
        0

