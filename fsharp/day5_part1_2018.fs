open System
open System.IO
open System.Collections.Generic

let react (s:string) =
    let stack = Stack<char>()
    for c in s do
        if stack.Count>0 && Char.ToLower(stack.Peek()) = Char.ToLower(c) && stack.Peek() <> c then
            stack.Pop() |> ignore
        else
            stack.Push(c)
    stack.Count

[<EntryPoint>]
let main _ =
    let polymer = File.ReadAllText("input.txt").TrimEnd('\r','\n')
    printfn "%d" (react polymer)
    0