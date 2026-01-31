
open System
open System.IO

let spin (arr: char[]) x =
    let n = arr.Length
    let tmp = Array.copy arr
    for i = 0 to n - 1 do
        arr.[(i + x) % n] <- tmp.[i]

let exchange (arr: char[]) a b =
    let t = arr.[a]
    arr.[a] <- arr.[b]
    arr.[b] <- t

let partner (arr: char[]) a b =
    let i = Array.findIndex ((=) a) arr
    let j = Array.findIndex ((=) b) arr
    exchange arr i j

[<EntryPoint>]
let main _ =
    let line = File.ReadAllText("input.txt").Trim()
    let moves = line.Split(',')
    let programs = [|'a'..'p'|]

    for m in moves do
        match m.[0] with
        | 's' ->
            let x = int m.[1..]
            spin programs x
        | 'x' ->
            let parts = m.[1..].Split('/')
            let a = int parts.[0]
            let b = int parts.[1]
            exchange programs a b
        | 'p' ->
            let a = m.[1]
            let b = m.[3]
            partner programs a b
        | _ -> ()

    Console.WriteLine(String(programs))
    0
