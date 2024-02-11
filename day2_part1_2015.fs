
module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let calculateSurfaceArea l w h =
    let sides = [l*w; w*h; h*l]
    let minSide = List.min sides
    let totalArea = 2*l*w + 2*w*h + 2*h*l
    totalArea + minSide

let totalSquareFeet = 
    input 
    |> Array.map (fun line -> 
        let dimensions = line.Split 'x' |> Array.map int
        let l = dimensions.[0]
        let w = dimensions.[1]
        let h = dimensions.[2]
        calculateSurfaceArea l w h)
    |> Array.sum

printfn "%d" totalSquareFeet
