
open System
open System.IO

type Cart = { mutable x:int; mutable y:int; mutable dir:char; mutable turn:int }

let readInput () =
    let lines = File.ReadAllLines("input.txt")
    let track = Array.init lines.Length (fun i -> lines.[i].ToCharArray())
    let carts = System.Collections.Generic.List<Cart>()
    for y in 0 .. lines.Length - 1 do
        for x in 0 .. lines.[y].Length - 1 do
            match track.[y].[x] with
            | '>' -> track.[y].[x] <- '-'; carts.Add({x=x; y=y; dir='>'; turn=0})
            | '<' -> track.[y].[x] <- '-'; carts.Add({x=x; y=y; dir='<'; turn=0})
            | '^' -> track.[y].[x] <- '|'; carts.Add({x=x; y=y; dir='^'; turn=0})
            | 'v' -> track.[y].[x] <- '|'; carts.Add({x=x; y=y; dir='v'; turn=0})
            | _ -> ()
    track, carts

let move (track:char[][]) (c:Cart) =
    match c.dir with
    | '>' ->
        let nx = c.x + 1
        match track.[c.y].[nx] with
        | '\\' -> c.dir <- 'v'
        | '/'   -> c.dir <- '^'
        | '+'   ->
            match c.turn with
            | 0 -> c.dir <- '^'; c.turn <- 1
            | 1 -> c.turn <- 2
            | _ -> c.dir <- 'v'; c.turn <- 0
        | _ -> ()
        c.x <- nx
    | '<' ->
        let nx = c.x - 1
        match track.[c.y].[nx] with
        | '/'   -> c.dir <- 'v'
        | '\\' -> c.dir <- '^'
        | '+'   ->
            match c.turn with
            | 0 -> c.dir <- 'v'; c.turn <- 1
            | 1 -> c.turn <- 2
            | _ -> c.dir <- '^'; c.turn <- 0
        | _ -> ()
        c.x <- nx
    | '^' ->
        let ny = c.y - 1
        match track.[ny].[c.x] with
        | '/'   -> c.dir <- '>'
        | '\\' -> c.dir <- '<'
        | '+'   ->
            match c.turn with
            | 0 -> c.dir <- '<'; c.turn <- 1
            | 1 -> c.turn <- 2
            | _ -> c.dir <- '>'; c.turn <- 0
        | _ -> ()
        c.y <- ny
    | 'v' ->
        let ny = c.y + 1
        match track.[ny].[c.x] with
        | '/'   -> c.dir <- '<'
        | '\\' -> c.dir <- '>'
        | '+'   ->
            match c.turn with
            | 0 -> c.dir <- '>'; c.turn <- 1
            | 1 -> c.turn <- 2
            | _ -> c.dir <- '<'; c.turn <- 0
        | _ -> ()
        c.y <- ny
    | _ -> ()

[<EntryPoint>]
let main argv =
    let track, carts = readInput()
    let mutable collision = false
    while not collision do
        carts.Sort(fun a b ->
            if a.y <> b.y then compare a.y b.y else compare a.x b.x)
        for c in carts do move track c
        let dict = System.Collections.Generic.Dictionary<int*int,int>()
        for i in 0 .. carts.Count-1 do
            let key = (carts.[i].x, carts.[i].y)
            if dict.ContainsKey(key) then
                collision <- true
                printfn "%d,%d" key.Item1 key.Item2
            else dict.[key] <- i
    0
