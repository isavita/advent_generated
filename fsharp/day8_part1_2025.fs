
open System
open System.IO

type Edge = {u:int; v:int; d:int64}

let main() =
    let pts =
        File.ReadAllLines "input.txt"
        |> Array.choose (fun l ->
            let a = l.Split(',')
            if a.Length = 3 then
                Some (int a.[0], int a.[1], int a.[2])
            else None)

    let n = pts.Length
    if n < 2 then
        printfn "Not enough points to form circuits."
        exit 0

    let edges =
        [| for i in 0..n-1 do
            let x1,y1,z1 = pts.[i]
            for j in i+1..n-1 do
                let x2,y2,z2 = pts.[j]
                let dx,dy,dz = int64 x1-int64 x2, int64 y1-int64 y2, int64 z1-int64 z2
                yield {u=i; v=j; d=dx*dx+dy*dy+dz*dz} |]
        |> Array.sortWith (fun a b -> compare a.d b.d)

    let parent = Array.init n id
    let sz = Array.create n 1

    let find x =
        let rec go x =
            if parent.[x] <> x then
                parent.[x] <- parent.[parent.[x]]
                go parent.[x]
            else x
        go x

    let union a b =
        let ra, rb = find a, find b
        if ra <> rb then
            if sz.[ra] < sz.[rb] then
                parent.[ra] <- rb
                sz.[rb] <- sz.[rb] + sz.[ra]
            else
                parent.[rb] <- ra
                sz.[ra] <- sz.[ra] + sz.[rb]

    let limit = min 1000 edges.Length
    for i in 0..limit-1 do
        union edges.[i].u edges.[i].v

    let top = Array.zeroCreate 3
    for i in 0..n-1 do
        if parent.[i] = i then
            let s = sz.[i]
            if s > top.[0] then
                top.[2] <- top.[1]
                top.[1] <- top.[0]
                top.[0] <- s
            elif s > top.[1] then
                top.[2] <- top.[1]
                top.[1] <- s
            elif s > top.[2] then
                top.[2] <- s

    let result = top |> Array.fold (fun acc v -> if v > 0 then acc * uint64 v else acc) 1UL
    printfn "Product of three largest circuit sizes: %d" result

main()
