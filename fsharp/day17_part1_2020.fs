
module Day17

let input = System.IO.File.ReadAllLines "input.txt"

let mutable cubes = Set.empty

for i in 0 .. input.Length - 1 do
    for j in 0 .. input.[0].Length - 1 do
        if input.[i].[j] = '#' then
            cubes <- cubes.Add (i, j, 0)

let mutable activeCubes = cubes

for _ in 1 .. 6 do
    let mutable newActiveCubes = Set.empty

    let mutable minX = System.Int32.MaxValue
    let mutable maxX = System.Int32.MinValue
    let mutable minY = System.Int32.MaxValue
    let mutable maxY = System.Int32.MinValue
    let mutable minZ = System.Int32.MaxValue
    let mutable maxZ = System.Int32.MinValue

    for (x, y, z) in activeCubes do
        minX <- min minX x
        maxX <- max maxX x
        minY <- min minY y
        maxY <- max maxY y
        minZ <- min minZ z
        maxZ <- max maxZ z

    for x in minX - 1 .. maxX + 1 do
        for y in minY - 1 .. maxY + 1 do
            for z in minZ - 1 .. maxZ + 1 do
                let mutable activeNeighbors = 0

                for dx in -1 .. 1 do
                    for dy in -1 .. 1 do
                        for dz in -1 .. 1 do
                            if dx <> 0 || dy <> 0 || dz <> 0 then
                                if activeCubes.Contains (x + dx, y + dy, z + dz) then
                                    activeNeighbors <- activeNeighbors + 1

                if activeCubes.Contains (x, y, z) then
                    if activeNeighbors = 2 || activeNeighbors = 3 then
                        newActiveCubes <- newActiveCubes.Add (x, y, z)
                else
                    if activeNeighbors = 3 then
                        newActiveCubes <- newActiveCubes.Add (x, y, z)

    activeCubes <- newActiveCubes

printfn "%d" (activeCubes.Count)
