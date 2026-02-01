
open System
open System.IO
open System.Text.RegularExpressions

[<Literal>]
let width = 50
[<Literal>]
let height = 6

let rect (screen: bool[][]) a b =
    for y in 0 .. b-1 do
        for x in 0 .. a-1 do
            screen.[y].[x] <- true

let rotateRow (screen: bool[][]) row shift =
    let tmp = Array.zeroCreate<bool> width
    for i in 0 .. width-1 do
        tmp.[(i + shift) % width] <- screen.[row].[i]
    screen.[row] <- tmp

let rotateColumn (screen: bool[][]) col shift =
    let tmp = Array.zeroCreate<bool> height
    for i in 0 .. height-1 do
        tmp.[(i + shift) % height] <- screen.[i].[col]
    for i in 0 .. height-1 do
        screen.[i].[col] <- tmp.[i]

let countLit (screen: bool[][]) =
    screen |> Array.sumBy (Array.fold (fun acc b -> acc + if b then 1 else 0) 0)

let rectRe = Regex(@"rect (\d+)x(\d+)")
let rowRe = Regex(@"rotate row y=(\d+) by (\d+)")
let colRe = Regex(@"rotate column x=(\d+) by (\d+)")

let process line (screen: bool[][]) =
    let m = rectRe.Match(line)
    if m.Success then
        rect screen (int m.Groups.[1].Value) (int m.Groups.[2].Value)
    else
        let m = rowRe.Match(line)
        if m.Success then
            rotateRow screen (int m.Groups.[1].Value) (int m.Groups.[2].Value)
        else
            let m = colRe.Match(line)
            if m.Success then
                rotateColumn screen (int m.Groups.[1].Value) (int m.Groups.[2].Value)

[<EntryPoint>]
let main _ =
    let screen = Array.init height (fun _ -> Array.create width false)
    use sr = new StreamReader("input.txt")
    let mutable line = sr.ReadLine()
    while not (isNull line) do
        process line screen
        line <- sr.ReadLine()
    printfn "%d" (countLit screen)
    0
