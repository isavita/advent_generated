
open System
open System.IO
open System.Text.RegularExpressions

type Vec3 = { x: int; y: int; z: int }

type Moon = { pos: Vec3; vel: Vec3 }

let abs x = if x < 0 then -x else x

let applyGravity (moons: Moon[]) =
    for i = 0 to moons.Length - 1 do
        for j = i + 1 to moons.Length - 1 do
            let mutable moonI = moons.[i]
            let mutable moonJ = moons.[j]

            if moonI.pos.x > moonJ.pos.x then
                moonI <- { moonI with vel = { moonI.vel with x = moonI.vel.x - 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with x = moonJ.vel.x + 1 } }
            elif moonI.pos.x < moonJ.pos.x then
                moonI <- { moonI with vel = { moonI.vel with x = moonI.vel.x + 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with x = moonJ.vel.x - 1 } }

            if moonI.pos.y > moonJ.pos.y then
                moonI <- { moonI with vel = { moonI.vel with y = moonI.vel.y - 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with y = moonJ.vel.y + 1 } }
            elif moonI.pos.y < moonJ.pos.y then
                moonI <- { moonI with vel = { moonI.vel with y = moonI.vel.y + 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with y = moonJ.vel.y - 1 } }

            if moonI.pos.z > moonJ.pos.z then
                moonI <- { moonI with vel = { moonI.vel with z = moonI.vel.z - 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with z = moonJ.vel.z + 1 } }
            elif moonI.pos.z < moonJ.pos.z then
                moonI <- { moonI with vel = { moonI.vel with z = moonI.vel.z + 1 } }
                moonJ <- { moonJ with vel = { moonJ.vel with z = moonJ.vel.z - 1 } }

            moons.[i] <- moonI
            moons.[j] <- moonJ

let applyVelocity (moons: Moon[]) =
    for i = 0 to moons.Length - 1 do
        let moon = moons.[i]
        moons.[i] <- { moon with pos = { x = moon.pos.x + moon.vel.x; y = moon.pos.y + moon.vel.y; z = moon.pos.z + moon.vel.z } }

let totalEnergy (moons: Moon[]) =
    moons
    |> Array.sumBy (fun moon ->
        let pot = abs moon.pos.x + abs moon.pos.y + abs moon.pos.z
        let kin = abs moon.vel.x + abs moon.vel.y + abs moon.vel.z
        pot * kin)

let parseMoon (line: string) =
    let regex = Regex(@"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
    let match' = regex.Match(line)
    if match'.Success then
        { pos = { x = int match'.Groups.[1].Value; y = int match'.Groups.[2].Value; z = int match'.Groups.[3].Value }; vel = { x = 0; y = 0; z = 0 } }
    else
        failwith "Invalid input"

let main () =
    try
        let lines = File.ReadAllLines("input.txt")
        let moons = Array.map parseMoon lines

        for _ = 1 to 1000 do
            applyGravity moons
            applyVelocity moons

        printfn "%d" (totalEnergy moons)
    with
    | ex -> printfn "Error: %s" ex.Message

main ()
