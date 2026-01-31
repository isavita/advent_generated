
open System
open System.IO

let [<Literal>] WIDTH = 7
let [<Literal>] NUM_ROCKS = 2022
let [<Literal>] NUM_SHAPES = 5
let [<Literal>] MAX_ROCK_POINTS = 5
let [<Literal>] CHAMBER_HEIGHT = 4000

let rockShapes = [|
    [| (0,0); (1,0); (2,0); (3,0) |]                 // -
    [| (1,0); (0,1); (1,1); (2,1); (1,2) |]          // +
    [| (0,0); (1,0); (2,0); (2,1); (2,2) |]          // ⌝
    [| (0,0); (0,1); (0,2); (0,3) |]                 // |
    [| (0,0); (1,0); (0,1); (1,1) |]                  // ■
|]

let chamber = Array2D.zeroCreate CHAMBER_HEIGHT WIDTH
let mutable highestY = 0

let inline isValid x y =
    x >= 0 && x < WIDTH && y > 0 && y < CHAMBER_HEIGHT && chamber[y,x] = 0uy

let canMove (rock: (int*int)[]) dx dy =
    let mutable ok = true
    let mutable i = 0
    while ok && i < rock.Length do
        let x,y = rock[i]
        ok <- isValid (x+dx) (y+dy)
        i <- i+1
    ok

let moveRock (rock: (int*int)[]) dx dy =
    for i = 0 to rock.Length-1 do
        let x,y = rock[i]
        rock[i] <- (x+dx, y+dy)

let settleRock (rock: (int*int)[]) =
    for i = 0 to rock.Length-1 do
        let x,y = rock[i]
        chamber[y,x] <- 1uy
        if y > highestY then highestY <- y

[<EntryPoint>]
let main _ =
    let jet = File.ReadAllText("input.txt").Trim()
    let jetLen = jet.Length
    let mutable jetIdx = 0

    for rockNum = 0 to NUM_ROCKS-1 do
        let shapeIdx = rockNum % NUM_SHAPES
        let shape = rockShapes[shapeIdx]
        let startX = 2
        let startY = highestY + 4
        let rock = Array.mapi (fun i (dx,dy) -> (startX+dx, startY+dy)) shape

        let rec fall () =
            // jet push
            let c = jet[jetIdx % jetLen]
            jetIdx <- jetIdx + 1
            let dx = if c = '>' then 1 else -1
            if canMove rock dx 0 then moveRock rock dx 0

            // down
            if canMove rock 0 -1 then
                moveRock rock 0 -1
                fall ()
            else
                settleRock rock
        fall ()
    printfn "%d" highestY
    0
