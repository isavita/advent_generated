
open System
open System.IO

let getModes n =
    let m1 = n % 10
    let m2 = (n / 10) % 10
    let m3 = (n / 100) % 10
    (m1, m2, m3)

let runProgram (memory:int[]) (inputs:int[]) =
    let mutable ip = 0
    let mutable inputIdx = 0
    let rec loop () =
        let opcode = memory.[ip] % 100
        let m1, m2, _ = getModes (memory.[ip] / 100)
        match opcode with
        | 1 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            memory.[memory.[ip+3]] <- a + b
            ip <- ip + 4
            loop ()
        | 2 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            memory.[memory.[ip+3]] <- a * b
            ip <- ip + 4
            loop ()
        | 3 ->
            memory.[memory.[ip+1]] <- inputs.[inputIdx]
            inputIdx <- inputIdx + 1
            ip <- ip + 2
            loop ()
        | 4 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            ip <- ip + 2
            a
        | 5 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            ip <- if a <> 0 then b else ip + 3
            loop ()
        | 6 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            ip <- if a = 0 then b else ip + 3
            loop ()
        | 7 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            memory.[memory.[ip+3]] <- if a < b then 1 else 0
            ip <- ip + 4
            loop ()
        | 8 ->
            let a = if m1 = 0 then memory.[memory.[ip+1]] else memory.[ip+1]
            let b = if m2 = 0 then memory.[memory.[ip+2]] else memory.[ip+2]
            memory.[memory.[ip+3]] <- if a = b then 1 else 0
            ip <- ip + 4
            loop ()
        | 99 -> 0
        | _ -> failwith "unknown opcode"
    loop ()

let rec permute lst =
    match lst with
    | [] -> [[]]
    | x::xs ->
        permute xs
        |> List.collect (fun perm ->
            List.init (perm.Length + 1) (fun i ->
                List.take i perm @ (x :: List.skip i perm)))

let phases = [0..4]
let perms = permute phases

let memoryBase =
    File.ReadAllText "input.txt"
    |> (fun s -> s.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map int

let maxOutput =
    perms
    |> List.map (fun seq ->
        let mutable out = 0
        for p in seq do
            out <- runProgram (Array.copy memoryBase) (Array.ofList [p; out])
        out)
    |> List.max

printfn "%d" maxOutput
