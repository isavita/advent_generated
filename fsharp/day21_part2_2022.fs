
open System
open System.IO
open System.Collections.Generic

type Monkey(name:string) =
    member val Name = name with get, set
    member val Val = 0L with get, set
    member val HasVal = false with get, set
    member val Left : Monkey option = None with get, set
    member val Right : Monkey option = None with get, set
    member val Op = '\000' with get, set

let map = Dictionary<string, Monkey>()

let findOrCreate (n:string) =
    match map.TryGetValue n with
    | true, m -> m
    | _ ->
        let m = Monkey n
        map.[n] <- m
        m

let rec trySolve (m:Monkey) : int64 option =
    if m.HasVal then Some m.Val
    else
        match m.Left, m.Right with
        | Some l, Some r ->
            match trySolve l, trySolve r with
            | Some lv, Some rv ->
                match m.Op with
                | '+' -> Some (lv + rv)
                | '-' -> Some (lv - rv)
                | '*' -> Some (lv * rv)
                | '/' -> if rv = 0L then None else Some (lv / rv)
                | '=' -> Some (if lv = rv then 1L else 0L)
                | _   -> None
            | _ -> None
        | _ -> None

let rec expect (m:Monkey) (target:int64) : int64 =
    if m.Name = "humn" then target
    else
        let lOk, lVal = 
            match m.Left with
            | Some l ->
                match trySolve l with
                | Some v -> true, v
                | None   -> false, 0L
            | None -> false, 0L
        let rOk, rVal =
            match m.Right with
            | Some r ->
                match trySolve r with
                | Some v -> true, v
                | None   -> false, 0L
            | None -> false, 0L
        if not lOk then
            match m.Op, m.Right with
            | '+', _ -> expect (Option.get m.Left) (target - rVal)
            | '-', _ -> expect (Option.get m.Left) (target + rVal)
            | '*', _ -> expect (Option.get m.Left) (target / rVal)
            | '/', _ -> expect (Option.get m.Left) (target * rVal)
            | '=', _ -> expect (Option.get m.Left) rVal
            | _ -> failwith "invalid"
        elif not rOk then
            match m.Op, m.Left with
            | '+', _ -> expect (Option.get m.Right) (target - lVal)
            | '-', _ -> expect (Option.get m.Right) (lVal - target)
            | '*', _ -> expect (Option.get m.Right) (target / lVal)
            | '/', _ -> expect (Option.get m.Right) (lVal / target)
            | '=', _ -> expect (Option.get m.Right) lVal
            | _ -> failwith "invalid"
        else failwith "cannot determine path"

[<EntryPoint>]
let main _ =
    map.Clear()
    let path = "input.txt"
    if not (File.Exists path) then
        Console.Error.WriteLine "Error opening input.txt"
        1
    else
        File.ReadLines(path)
        |> Seq.iter (fun line ->
            let s = line.Trim()
            if s <> "" then
                let parts = s.Split([|':'|], 2)
                let name = parts.[0].Trim()
                let rest = parts.[1].Trim()
                match Int64.TryParse rest with
                | true, v ->
                    let m = findOrCreate name
                    m.Val <- v
                    m.HasVal <- true
                | _ ->
                    let toks = rest.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
                    let leftName = toks.[0]
                    let op = toks.[1].[0]
                    let rightName = toks.[2]
                    let m = findOrCreate name
                    m.Left <- Some (findOrCreate leftName)
                    m.Right <- Some (findOrCreate rightName)
                    m.Op <- op
                    m.HasVal <- false)
        let root = findOrCreate "root"
        let humn = findOrCreate "humn"
        root.Op <- '='
        humn.HasVal <- false
        let ans = expect root 0L
        printfn "%d" ans
        0
