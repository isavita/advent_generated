
open System
open System.IO

[<EntryPoint>]
let main argv =
    let iterations = 50
    let expandBy = 1
    let lines = File.ReadAllLines "input.txt"
    let algorithm = lines.[0]
    let mutable image =
        lines
        |> Array.skip 2
        |> Array.map (fun line -> line |> Seq.map (fun c -> c = '#') |> Seq.toArray)

    let enhance (algorithm:string) (image:bool[][]) useInfiniteLit =
        let h = image.Length
        let w = image.[0].Length
        let newH = h + (expandBy * 2)
        let newW = w + (expandBy * 2)
        let newImage = Array.init newH (fun _ -> Array.create newW false)
        for y in -expandBy .. h + expandBy - 1 do
            for x in -expandBy .. w + expandBy - 1 do
                let mutable index = 0
                for dy in -1 .. 1 do
                    for dx in -1 .. 1 do
                        index <- index <<< 1
                        let ny = y + dy
                        let nx = x + dx
                        if ny >= 0 && ny < h && nx >= 0 && nx < w then
                            if image.[ny].[nx] then index <- index ||| 1
                        elif useInfiniteLit then
                            index <- index ||| 1
                newImage.[y + expandBy].[x + expandBy] <- algorithm.[index] = '#'
        newImage

    let countLit (image:bool[][]) =
        image
        |> Array.sumBy (fun row -> row |> Array.sumBy (fun b -> if b then 1 else 0))

    for i in 0 .. iterations - 1 do
        image <- enhance algorithm image (i % 2 = 1 && algorithm.[0] = '#')

    printfn "%d" (countLit image)
    0
