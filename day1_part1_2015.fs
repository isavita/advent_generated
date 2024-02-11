module Day1

let input = System.IO.File.ReadAllText("input.txt")

let floor = input.ToCharArray()
             |> Array.fold (fun acc c -> if c = '(' then acc + 1 else acc - 1) 0

printfn "%d" floor