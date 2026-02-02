
open System.IO

let readHeightmap (filename: string) =
    File.ReadAllLines(filename)
    |> Array.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> List.ofSeq)

let isLowPoint (heightmap: int list array) x y =
    let height = heightmap.[y].[x]
    (x = 0 || height < heightmap.[y].[x - 1]) &&
    (x = heightmap.[y].Length - 1 || height < heightmap.[y].[x + 1]) &&
    (y = 0 || height < heightmap.[y - 1].[x]) &&
    (y = heightmap.Length - 1 || height < heightmap.[y + 1].[x])

let sumRiskLevels (heightmap: int list array) =
    seq {
        for y in 0 .. heightmap.Length - 1 do
        for x in 0 .. heightmap.[y].Length - 1 do
        if isLowPoint heightmap x y then
            yield 1 + heightmap.[y].[x]
    } |> Seq.sum

let main () =
    let heightmap = readHeightmap "input.txt"
    printfn "%d" (sumRiskLevels heightmap)

main ()
