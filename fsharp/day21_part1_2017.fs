
open System
open System.IO
open System.Collections.Generic

let split (s:string) = s.Split('/')
let join (a:string[]) = String.Join("/", a)

let rotate (p:string) =
    let rows = split p
    let n = rows.Length
    let newRows = Array.init n (fun i ->
        rows |> Array.map (fun r -> r.[n-1-i]) |> String)
    join newRows

let flip (p:string) =
    split p |> Array.map (fun r -> new string (r.ToCharArray() |> Array.rev)) |> join

let addVariants (input:string) (output:string) (dict:Dictionary<string,string>) =
    let mutable cur = input
    for _ in 0 .. 3 do
        dict.[cur] <- output
        cur <- rotate cur
    let flipped = flip input
    cur <- flipped
    for _ in 0 .. 3 do
        dict.[cur] <- output
        cur <- rotate cur

let readRules () =
    let dict = Dictionary<string,string>()
    for line in File.ReadLines("input.txt") do
        match line.IndexOf(" => ") with
        | -1 -> ()
        | i ->
            let inp = line.Substring(0,i)
            let outp = line.Substring(i+4).Trim()
            addVariants inp outp dict
    dict

let extract (grid:char[][]) (y:int) (x:int) (size:int) =
    let sb = System.Text.StringBuilder()
    for dy in 0 .. size-1 do
        for dx in 0 .. size-1 do
            sb.Append(grid.[y+dy].[x+dx]) |> ignore
        sb.Append('/') |> ignore
    sb.Length <- sb.Length-1
    sb.ToString()

let apply (grid:char[][]) (rules:Dictionary<string,string>) =
    let n = grid.Length
    let sub = if n%2=0 then 2 else 3
    let newSize = n/sub*(sub+1)
    let newGrid = Array.init newSize (fun _ -> Array.create newSize '.')
    for gy in 0 .. sub .. n-1 do
        for gx in 0 .. sub .. n-1 do
            let pat = extract grid gy gx sub
            let outPat = rules.[pat]
            let rows = split outPat
            let baseY = (gy/sub)*(sub+1)
            let baseX = (gx/sub)*(sub+1)
            for dy in 0 .. rows.Length-1 do
                for dx in 0 .. rows.[dy].Length-1 do
                    newGrid.[baseY+dy].[baseX+dx] <- rows.[dy].[dx]
    newGrid

[<EntryPoint>]
let main _ =
    let rules = readRules()
    let mutable grid = [| ".#.".ToCharArray(); "..#".ToCharArray(); "###".ToCharArray() |]
    for _ in 1 .. 5 do
        grid <- apply grid rules
    let count = grid |> Array.sumBy (fun row -> row |> Array.filter ((=) '#') |> Array.length)
    printfn "%d" count
    0
