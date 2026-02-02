
open System
open System.Collections.Generic
open System.IO

type Point3D = { x: int; y: int; z: int }

let directions = 
    [| (-1, 0, 0); (1, 0, 0); (0, -1, 0); (0, 1, 0); (0, 0, -1); (0, 0, 1) |]

let readInput (filePath: string) =
    File.ReadAllLines(filePath)
    |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))
    |> Seq.map (fun line -> 
        let parts = line.Split(',')
        { x = int parts.[0]; y = int parts.[1]; z = int parts.[2] })

let getBounds (cubes: seq<Point3D>) =
    let minX = Seq.minBy (fun cube -> cube.x) cubes |> (fun cube -> cube.x)
    let maxX = Seq.maxBy (fun cube -> cube.x) cubes |> (fun cube -> cube.x)
    let minY = Seq.minBy (fun cube -> cube.y) cubes |> (fun cube -> cube.y)
    let maxY = Seq.maxBy (fun cube -> cube.y) cubes |> (fun cube -> cube.y)
    let minZ = Seq.minBy (fun cube -> cube.z) cubes |> (fun cube -> cube.z)
    let maxZ = Seq.maxBy (fun cube -> cube.z) cubes |> (fun cube -> cube.z)
    (minX - 1, maxX + 1, minY - 1, maxY + 1, minZ - 1, maxZ + 1)

let countFaces (cubes: HashSet<Point3D>) (minX, maxX, minY, maxY, minZ, maxZ) =
    let seen = new HashSet<Point3D>()
    let q = new Queue<Point3D>()
    q.Enqueue({ x = minX; y = minY; z = minZ })
    seen.Add({ x = minX; y = minY; z = minZ })

    let rec bfs () =
        if q.Count = 0 then 0
        else
            let curr = q.Dequeue()
            let count = 
                directions 
                |> Array.sumBy (fun (dx, dy, dz) ->
                    let nx, ny, nz = curr.x + dx, curr.y + dy, curr.z + dz
                    if nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ then 0
                    else if cubes.Contains({ x = nx; y = ny; z = nz }) then 1
                    else if seen.Contains({ x = nx; y = ny; z = nz }) then 0
                    else 
                        seen.Add({ x = nx; y = ny; z = nz })
                        q.Enqueue({ x = nx; y = ny; z = nz })
                        0)
            count + bfs ()

    bfs ()

[<EntryPoint>]
let main argv =
    let cubes = 
        readInput "input.txt" 
        |> Seq.map (fun cube -> { x = cube.x; y = cube.y; z = cube.z }) 
        |> HashSet

    let bounds = getBounds cubes
    let faces = countFaces cubes bounds
    printfn "%d" faces
    0
