
open System
open System.IO

type Robot = { x:int; y:int; vx:int; vy:int }

let mod' a b = (a % b + b) % b

let parse (s:string) =
    let p = s.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let px = p[0].Substring(2).Split(',') |> fun a -> int a[0]
    let py = p[0].Substring(2).Split(',') |> fun a -> int a[1]
    let vx = p[1].Substring(2).Split(',') |> fun a -> int a[0]
    let vy = p[1].Substring(2).Split(',') |> fun a -> int a[1]
    { x=px; y=py; vx=vx; vy=vy }

let step sizeX sizeY (r:Robot) =
    { r with x = mod' (r.x + r.vx) sizeX
             y = mod' (r.y + r.vy) sizeY }

let safety sizeX sizeY (bots:Robot[]) =
    let cx, cy = sizeX / 2, sizeY / 2
    let mutable q1,q2,q3,q4 = 0,0,0,0
    for b in bots do
        if b.x < cx then
            if b.y < cy then q1 <- q1 + 1
            elif b.y > cy then q2 <- q2 + 1
        elif b.x > cx then
            if b.y < cy then q3 <- q3 + 1
            elif b.y > cy then q4 <- q4 + 1
    q1 * q2 * q3 * q4

let unique (bots:Robot[]) =
    let seen = System.Collections.Generic.HashSet<int*int>()
    bots |> Array.forall (fun b -> seen.Add(b.x, b.y))

let robots = File.ReadAllLines("input.txt") |> Array.map parse
let mutable bots = Array.copy robots

for _ in 1..100 do
    bots <- Array.map (step 101 103) bots
printfn "%d" (safety 101 103 bots)

bots <- Array.copy robots
let mutable t = 0
while not (unique bots) do
    bots <- Array.map (step 101 103) bots
    t <- t + 1
printfn "%d" t
