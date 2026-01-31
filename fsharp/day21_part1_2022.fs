
open System
open System.IO
open System.Collections.Generic

let lines = File.ReadAllLines "input.txt"

let jobs = Dictionary<string,string>()

for l in lines do
    let i = l.IndexOf(':')
    if i >= 0 then
        let name = l.Substring(0,i)
        let job = l.Substring(i+2).Trim()
        jobs[name] <- job

let rec eval (monkey: string) : int64 =
    let job = jobs[monkey]
    match Int64.TryParse job with
    | true, n -> n
    | _ ->
        let parts = job.Split(' ')
        let a = eval parts[0]
        let b = eval parts[2]
        match parts[1] with
        | "+" -> a + b
        | "-" -> a - b
        | "*" -> a * b
        | "/" -> a / b
        | _ -> failwith "bad op"

printfn "%d" (eval "root")
