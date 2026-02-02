
open System
open System.IO

let readHeightmap() =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line |> Seq.map (fun c -> int c - int '0') |> Seq.toArray)
    |> Array.toSeq

let isLowPoint (heightmap: int[][]) x y =
    let height = heightmap.[y].[x]
    (x = 0 || height < heightmap.[y].[x - 1]) &&
    (x = heightmap.[y].Length - 1 || height < heightmap.[y].[x + 1]) &&
    (y = 0 || height < heightmap.[y - 1].[x]) &&
    (y = heightmap.Length - 1 || height < heightmap.[y + 1].[x])

let exploreBasin (heightmap: int[][]) (visited: bool[,]) x y =
    let rec explore x y =
        if x < 0 || x >= heightmap.[0].Length || y < 0 || y >= heightmap.Length || visited.[y, x] || heightmap.[y].[x] = 9 then
            0
        else
            visited.[y, x] <- true
            1 + (explore (x - 1) y) + (explore (x + 1) y) + (explore x (y - 1)) + (explore x (y + 1))
    explore x y

let main() =
    let heightmap = readHeightmap() |> Seq.toArray
    let visited = Array2D.zeroCreate heightmap.Length heightmap.[0].Length
    let basinSizes =
        seq { for y in 0 .. heightmap.Length - 1 do
              for x in 0 .. heightmap.[y].Length - 1 do
              if isLowPoint heightmap x y then
                  yield exploreBasin heightmap visited x y }
        |> Seq.toArray
        |> Array.sort
    let result = basinSizes.[basinSizes.Length - 1] * basinSizes.[basinSizes.Length - 2] * basinSizes.[basinSizes.Length - 3]
    printfn "%d" result

main()
