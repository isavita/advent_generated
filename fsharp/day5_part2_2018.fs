
open System
open System.IO

let react (s: string) =
    let stack = System.Collections.Generic.Stack<char>()
    for c in s do
        if stack.Count > 0 && Char.ToUpperInvariant(stack.Peek()) = Char.ToUpperInvariant(c) && stack.Peek() <> c then
            stack.Pop() |> ignore
        else
            stack.Push(c)
    stack.Count

[<EntryPoint>]
let main argv =
    let polymer = File.ReadAllText("input.txt").Trim()
    let minLen =
        seq {
            for u = 'a' to 'z' do
                let filtered = polymer |> Seq.filter (fun c -> Char.ToLowerInvariant(c) <> u)
                yield react (String.Concat filtered)
        } |> Seq.min
    printfn "%d" minLen
    0
