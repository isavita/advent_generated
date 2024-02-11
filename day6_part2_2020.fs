
module Day6

let input = System.IO.File.ReadAllText("input.txt")

let groups = input.Split("\n\n")

let part1 = groups
            |> Array.map (fun group -> group.Replace("\n", ""))
            |> Array.map (fun group -> group.ToCharArray() |> Set.ofArray |> Set.count)
            |> Array.sum

let part2 = groups
            |> Array.map (fun group -> group.Split("\n") |> Array.map (fun person -> person.ToCharArray() |> Set.ofArray))
            |> Array.map (fun group -> group.[1..] |> Array.fold (Set.intersect) group.[0] |> Set.count)
            |> Array.sum

printfn "%d" part1
printfn "%d" part2
