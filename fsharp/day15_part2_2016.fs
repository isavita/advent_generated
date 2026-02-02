
open System
open System.IO
open System.Text.RegularExpressions

type Disc = 
    { totalPositions: int; startPosition: int }

let readInput (filename: string) : Disc array =
    File.ReadAllLines(filename)
    |> Array.map (fun line ->
        let matchResult = Regex.Match(line, @"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).")
        { totalPositions = int matchResult.Groups.[2].Value; startPosition = int matchResult.Groups.[3].Value })

let checkDiscs (discs: Disc array) (time: int) : bool =
    discs
    |> Array.mapi (fun i disc -> (disc.startPosition + time + i + 1) % disc.totalPositions = 0)
    |> Array.forall id

let main () =
    let discs = readInput "input.txt"
    let discs = Array.append discs [| { totalPositions = 11; startPosition = 0 } |]
    let rec loop time =
        if checkDiscs discs time then time
        else loop (time + 1)
    printfn "%d" (loop 0)

main ()
