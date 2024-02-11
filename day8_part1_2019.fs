
module Day8

let input = System.IO.File.ReadAllText("input.txt")

let width = 25
let height = 6
let layerSize = width * height

let layers = input
            |> Seq.chunkBySize layerSize
            |> Seq.map (fun layer -> layer |> Seq.countBy id)
            |> Seq.toList

let layerWithFewestZeros = layers
                            |> List.minBy (fun layer -> Seq.tryFind (fun (digit, count) -> digit = '0') layer |> Option.defaultValue ('0', 0) |> snd)

let ones = Seq.tryFind (fun (digit, count) -> digit = '1') layerWithFewestZeros |> Option.defaultValue ('1', 0) |> snd
let twos = Seq.tryFind (fun (digit, count) -> digit = '2') layerWithFewestZeros |> Option.defaultValue ('2', 0) |> snd

printfn "%d" (ones * twos)
