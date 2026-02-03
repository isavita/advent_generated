
open System
open System.IO
open System.Collections.Generic

type Rule = { Cat: char; Op: char; Num: int; Next: string }
type Interval = { Start: int; End: int }

let parseWorkflow (line:string) =
    let i = line.IndexOf('{')
    let name = line.Substring(0,i)
    let body = line.Substring(i+1,line.Length-i-2)
    let rules =
        body.Split(',')
        |> Array.map (fun part ->
            let j = part.IndexOf(':')
            if j = -1 then { Cat=' '; Op=' '; Num=0; Next=part }
            else
                let cat = part.[0]
                let op = part.[1]
                let num = Int32.Parse(part.Substring(2,j-2))
                let next = part.Substring(j+1)
                { Cat=cat; Op=op; Num=num; Next=next })
        |> List.ofArray
    name, rules

let parsePart (line:string) =
    let vals =
        line.Trim([|'{';'}'|]).Split(',')
        |> Array.map (fun p -> Int32.Parse(p.Split('=')[1]))
    dict [ ('x', vals.[0]); ('m', vals.[1]); ('a', vals.[2]); ('s', vals.[3]) ]

let rec apply (part:Map<char,Interval>) (wf:Map<string,Rule list>) name =
    if name = "A" then
        part.Values
        |> Seq.map (fun iv -> int64 (iv.End - iv.Start + 1))
        |> Seq.reduce (*)
    elif name = "R" then 0L
    else
        let rec loop rules cur total =
            match rules with
            | [] -> total
            | r::rs when r.Cat = ' ' ->
                let total = total + apply cur wf r.Next
                loop rs cur total
            | r::rs ->
                let curIv = cur.[r.Cat]
                let valid, invalid =
                    if r.Op = '>' then
                        { Start = max curIv.Start (r.Num+1); End = curIv.End },
                        { Start = curIv.Start; End = min curIv.End r.Num }
                    else
                        { Start = curIv.Start; End = min curIv.End (r.Num-1) },
                        { Start = max curIv.Start r.Num; End = curIv.End }
                let total = total + apply (cur.Add(r.Cat,valid)) wf r.Next
                if invalid.Start > invalid.End then total
                else loop rs (cur.Add(r.Cat,invalid)) total
        loop wf.[name] part 0L

let solve (lines:string[]) =
    let sep = lines |> Array.findIndex String.IsNullOrWhiteSpace
    let wfLines = lines.[0..sep-1]
    let partLines = lines.[sep+1..]
    let wfMap =
        wfLines
        |> Array.map parseWorkflow
        |> Map.ofArray
    let start =
        Map.ofList [ ('x',{Start=1;End=4000}); ('m',{Start=1;End=4000});
                     ('a',{Start=1;End=4000}); ('s',{Start=1;End=4000}) ]
    apply start wfMap "in"

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines("input.txt")
    printfn "%d" (solve lines)
    0
