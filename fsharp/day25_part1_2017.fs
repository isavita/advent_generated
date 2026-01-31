
open System
open System.IO
open System.Text.RegularExpressions

type Cmd = {write:int; move:int; next:char}
type State = Cmd array

let parse (path:string) =
    let lines = File.ReadAllLines(path)
    let initState = lines.[0].[15]
    let steps = Regex.Match(lines.[1], @"\d+").Value |> int
    let dict = System.Collections.Generic.Dictionary<char, State>()
    let mutable i = 2
    while i < lines.Length do
        if lines.[i].StartsWith("In state") then
            let state = lines.[i].[9]
            let cmds = Array.zeroCreate<Cmd> 2
            for v in 0..1 do
                let write = Regex.Match(lines.[i+2+4*v], @"\d+").Value |> int
                let move = if lines.[i+3+4*v].Contains("right") then 1 else -1
                let next = lines.[i+4+4*v].[26]
                cmds.[v] <- {write=write; move=move; next=next}
            dict.[state] <- cmds
            i <- i + 10
        else i <- i + 1
    initState, steps, dict

[<EntryPoint>]
let main argv =
    let initState, steps, states = parse "input.txt"
    let size = 20000
    let tape = Array.zeroCreate<int> size
    let mutable pos = size/2
    let mutable cur = initState
    for _ in 1..steps do
        let value = tape.[pos]
        let cmd = states.[cur].[value]
        tape.[pos] <- cmd.write
        pos <- pos + cmd.move
        cur <- cmd.next
    let checksum = Array.sum tape
    printfn "%d" checksum
    0
