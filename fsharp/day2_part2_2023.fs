
open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let gameRe = Regex(@"Game (\d+): (.+)", RegexOptions.Compiled)
    let cubeRe = Regex(@"(\d+) (red|green|blue)", RegexOptions.Compiled)

    File.ReadAllLines "input.txt"
    |> Array.sumBy (fun line ->
        let m = gameRe.Match line
        if not m.Success then 0 else

        let mutable maxR, maxG, maxB = 0, 0, 0
        for round in m.Groups[2].Value.Split(';') do
            let mutable r, g, b = 0, 0, 0
            for m2 in cubeRe.Matches round do
                let n = int m2.Groups[1].Value
                match m2.Groups[2].Value with
                | "red"   -> r <- r + n
                | "green" -> g <- g + n
                | "blue"  -> b <- b + n
                | _ -> ()
            maxR <- max maxR r
            maxG <- max maxG g
            maxB <- max maxB b
        maxR * maxG * maxB )
    |> printfn "%d"
    0
