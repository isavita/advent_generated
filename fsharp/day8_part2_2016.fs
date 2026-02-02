
open System
open System.IO

let screenWidth = 50
let screenHeight = 6

let initScreen () =
    Array.init screenHeight (fun _ -> Array.zeroCreate screenWidth)

let rect (screen: char[][]) a b =
    for y in 0 .. b - 1 do
        for x in 0 .. a - 1 do
            screen.[y].[x] <- '#'

let rotateRow (screen: char[][]) row shift =
    let temp = Array.init screenWidth (fun i -> screen.[row].[i])
    for i in 0 .. screenWidth - 1 do
        screen.[row].[(i + shift) % screenWidth] <- temp.[i]

let rotateColumn (screen: char[][]) col shift =
    let temp = Array.init screenHeight (fun i -> screen.[i].[col])
    for i in 0 .. screenHeight - 1 do
        screen.[(i + shift) % screenHeight].[col] <- temp.[i]

let processInstruction (screen: char[][]) (instruction: string) =
    let parts = instruction.Split ' '
    match parts.[0] with
    | "rect" ->
        let a, b = parts.[1].Split 'x' |> Array.map int |> (fun x -> x.[0], x.[1])
        rect screen a b
    | "rotate" ->
        match parts.[1] with
        | "row" ->
            let row = parts.[2].Split '=' |> (fun x -> int x.[1])
            let shift = int parts.[4]
            rotateRow screen row shift
        | "column" ->
            let col = parts.[2].Split '=' |> (fun x -> int x.[1])
            let shift = int parts.[4]
            rotateColumn screen col shift
        | _ -> failwith "Invalid instruction"
    | _ -> failwith "Invalid instruction"

let displayScreen (screen: char[][]) =
    for row in screen do
        printfn "%s" (String.concat "" (Array.map (fun c -> if c = '#' then "#" else ".") row))

let countLitPixels (screen: char[][]) =
    screen |> Array.sumBy (fun row -> Array.sumBy (fun c -> if c = '#' then 1 else 0) row)

let main () =
    let screen = initScreen ()
    let instructions = File.ReadAllLines "input.txt"
    instructions |> Array.iter (processInstruction screen)
    displayScreen screen
    printfn "Lit pixels: %d" (countLitPixels screen)

main ()
