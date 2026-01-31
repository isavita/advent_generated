
open System
open System.IO

let isValid (pwd: char[]) =
    let mutable straight = false
    let mutable pairs = 0
    let mutable prev = '\000'
    let mutable prev2 = '\000'
    for i = 0 to pwd.Length - 1 do
        let c = pwd.[i]
        if c = 'i' || c = 'o' || c = 'l' then
            straight <- false
            pairs <- -1                     // force invalid
        if i >= 2 && pwd.[i-1] = char (int c - 1) && pwd.[i-2] = char (int c - 2) then
            straight <- true
        if c = prev && c <> prev2 then
            pairs <- pairs + 1
            prev <- '\000'
        elif c = prev then
            prev <- '\000'
        else
            prev2 <- prev
            prev <- c
    straight && pairs >= 2

let increment (pwd: char[]) =
    let mutable i = pwd.Length - 1
    while i >= 0 do
        if pwd.[i] = 'z' then
            pwd.[i] <- 'a'
            i <- i - 1
        else
            pwd.[i] <- char (int pwd.[i] + 1)
            i <- -1

[<EntryPoint>]
let main _ =
    let pwd = File.ReadAllText("input.txt").Trim().ToCharArray()
    let mutable password = pwd
    while not (isValid password) do
        increment password
    printf "%s" (String(password))
    0
