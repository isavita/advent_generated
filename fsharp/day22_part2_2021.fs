
open System
open System.IO
open System.Text.RegularExpressions

type Cube = 
    { isOn: bool; x1: int; x2: int; y1: int; y2: int; z1: int; z2: int }

let cubeVolume (c: Cube) =
    let vol = int64 (c.x2 - c.x1 + 1) * int64 (c.y2 - c.y1 + 1) * int64 (c.z2 - c.z1 + 1)
    if c.isOn then vol else -vol

let getIntersection (c1: Cube) (c2: Cube) =
    let x1 = max c1.x1 c2.x1
    let x2 = min c1.x2 c2.x2
    let y1 = max c1.y1 c2.y1
    let y2 = min c1.y2 c2.y2
    let z1 = max c1.z1 c2.z1
    let z2 = min c1.z2 c2.z2

    if x1 > x2 || y1 > y2 || z1 > z2 then
        None
    else
        let intersectionState =
            match (c1.isOn, c2.isOn) with
            | true, true -> false
            | false, false -> true
            | _, state -> state

        Some { isOn = intersectionState; x1 = x1; x2 = x2; y1 = y1; y2 = y2; z1 = z1; z2 = z2 }

let parseInput (input: string) =
    let lines = input.Split('\n')
    lines
    |> Array.map (fun line ->
        let m = Regex.Match(line, @"^(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)$")
        if not m.Success then
            failwithf "Parsing error in line: %s" line

        let onOff = m.Groups.[1].Value = "on"
        let x1, x2 = int m.Groups.[2].Value, int m.Groups.[3].Value
        let y1, y2 = int m.Groups.[4].Value, int m.Groups.[5].Value
        let z1, z2 = int m.Groups.[6].Value, int m.Groups.[7].Value

        if x1 > x2 || y1 > y2 || z1 > z2 then
            failwithf "Invalid range in line: %s" line

        { isOn = onOff; x1 = x1; x2 = x2; y1 = y1; y2 = y2; z1 = z1; z2 = z2 })

let solve (cubes: Cube[]) =
    let mutable finalList = []
    for c in cubes do
        let toAdd =
            finalList
            |> List.choose (fun x -> getIntersection x c)
            |> (if c.isOn then fun x -> c :: x else id)
        finalList <- toAdd @ finalList

    finalList |> List.sumBy cubeVolume

let main () =
    try
        let input = File.ReadAllText "input.txt"
        let cubes = parseInput input
        let result = solve cubes
        printfn "%d" result
    with ex ->
        printfn "Error: %s" ex.Message
        Environment.Exit 1

main ()
