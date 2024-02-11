
module Day2

let input = System.IO.File.ReadAllLines "input.txt"

let totalWrappingPaper =
    input
    |> Array.sumBy (fun line ->
        let dimensions = line.Split 'x' |> Array.map int
        let l, w, h = dimensions.[0], dimensions.[1], dimensions.[2]
        let sides = [| l*w; w*h; h*l |]
        let slack = Array.min sides
        2*l*w + 2*w*h + 2*h*l + slack
    )

let totalRibbon =
    input
    |> Array.sumBy (fun line ->
        let dimensions = line.Split 'x' |> Array.map int
        let l, w, h = dimensions.[0], dimensions.[1], dimensions.[2]
        let smallestPerimeter = [| 2*l+2*w; 2*w+2*h; 2*h+2*l |] |> Array.min
        let volume = l * w * h
        smallestPerimeter + volume
    )

printfn "%A" totalWrappingPaper
printfn "%A" totalRibbon
