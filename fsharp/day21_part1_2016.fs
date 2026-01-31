
open System
open System.IO
open System.Collections.Generic

let swapPos (a: ResizeArray<char>) i j =
    let t = a.[i]
    a.[i] <- a.[j]
    a.[j] <- t

let swapLetter (a: ResizeArray<char>) x y =
    for i = 0 to a.Count - 1 do
        if a.[i] = x then a.[i] <- y
        elif a.[i] = y then a.[i] <- x

let rotateLeft (a: ResizeArray<char>) steps =
    let n = steps % a.Count
    for _ = 1 to n do
        let first = a.[0]
        a.RemoveAt(0)
        a.Add(first)

let rotateRight (a: ResizeArray<char>) steps =
    let n = steps % a.Count
    for _ = 1 to n do
        let last = a.[a.Count - 1]
        a.RemoveAt(a.Count - 1)
        a.Insert(0, last)

let rotateBasedOnPosition (a: ResizeArray<char>) x =
    let idx = a.IndexOf x
    let steps = 1 + idx + (if idx >= 4 then 1 else 0)
    rotateRight a steps

let reversePositions (a: ResizeArray<char>) i j =
    let mutable x = i
    let mutable y = j
    while x < y do
        swapPos a x y
        x <- x + 1
        y <- y - 1

let movePosition (a: ResizeArray<char>) i j =
    let ch = a.[i]
    a.RemoveAt(i)
    a.Insert(j, ch)

[<EntryPoint>]
let main _ =
    let pwd = ResizeArray<char>( "abcdefgh".ToCharArray() )
    let lines = File.ReadAllLines "input.txt"

    for line in lines do
        match line with
        | s when s.StartsWith("swap position") ->
            let p = s.Split(' ')
            swapPos pwd (int p.[2]) (int p.[5])
        | s when s.StartsWith("swap letter") ->
            let p = s.Split(' ')
            swapLetter pwd p.[2].[0] p.[5].[0]
        | s when s.StartsWith("rotate left") ->
            let p = s.Split(' ')
            rotateLeft pwd (int p.[2])
        | s when s.StartsWith("rotate right") ->
            let p = s.Split(' ')
            rotateRight pwd (int p.[2])
        | s when s.StartsWith("rotate based on position of letter") ->
            let p = s.Split(' ')
            rotateBasedOnPosition pwd p.[6].[0]
        | s when s.StartsWith("reverse positions") ->
            let p = s.Split(' ')
            reversePositions pwd (int p.[2]) (int p.[4])
        | s when s.StartsWith("move position") ->
            let p = s.Split(' ')
            movePosition pwd (int p.[2]) (int p.[5])
        | _ -> ()

    Console.WriteLine(String(pwd.ToArray()))
    0
