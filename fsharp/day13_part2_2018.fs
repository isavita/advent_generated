
open System
open System.IO
open System.Collections.Generic

type Cart() =
    member val X = 0 with get, set
    member val Y = 0 with get, set
    member val Dir = ' ' with get, set
    member val Turns = 0 with get, set
    member val Removed = false with get, set
    member this.Move(track: char[][]) =
        match this.Dir with
        | '>' -> this.X <- this.X + 1
        | '<' -> this.X <- this.X - 1
        | '^' -> this.Y <- this.Y - 1
        | 'v' -> this.Y <- this.Y + 1
        | _ -> ()
        let cell = track.[this.Y].[this.X]
        if cell = '+' then
            match this.Turns % 3 with
            | 0 ->
                this.Dir <-
                    match this.Dir with
                    | '>' -> '^' | '<' -> 'v' | '^' -> '<' | 'v' -> '>'
                    | _ -> this.Dir
            | 2 ->
                this.Dir <-
                    match this.Dir with
                    | '>' -> 'v' | '<' -> '^' | '^' -> '>' | 'v' -> '<'
                    | _ -> this.Dir
            | _ -> ()
            this.Turns <- this.Turns + 1
        elif cell = '/' then
            this.Dir <-
                match this.Dir with
                | '>' -> '^' | '<' -> 'v' | '^' -> '>' | 'v' -> '<'
                | _ -> this.Dir
        elif cell = '\\' then
            this.Dir <-
                match this.Dir with
                | '>' -> 'v' | '<' -> '^' | '^' -> '<' | 'v' -> '>'
                | _ -> this.Dir

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    let track = lines |> Array.map (fun l -> l.ToCharArray())
    let carts = ResizeArray<Cart>()
    for y = 0 to track.Length - 1 do
        for x = 0 to track.[y].Length - 1 do
            match track.[y].[x] with
            | '>' | '<' | '^' | 'v' as c ->
                let cart = Cart()
                cart.X <- x
                cart.Y <- y
                cart.Dir <- c
                carts.Add cart
                track.[y].[x] <- if c = '>' || c = '<' then '-' else '|'
            | _ -> ()
    while carts.Count > 1 do
        let ordered = carts |> Seq.sortBy (fun c -> c.Y, c.X) |> Seq.toArray
        for cart in ordered do
            if not cart.Removed then
                cart.Move track
                let collided = carts |> Seq.tryFind (fun c -> not c.Removed && c <> cart && c.X = cart.X && c.Y = cart.Y)
                match collided with
                | Some c ->
                    cart.Removed <- true
                    c.Removed <- true
                | None -> ()
        carts.RemoveAll(fun c -> c.Removed) |> ignore
    let last = carts.[0]
    printfn "%d,%d" last.X last.Y
    0
