
open System
open System.IO
open System.Collections.Generic

let floors = 4

type State = { Elevator:int; Gen:int[]; Chip:int[]; Steps:int }

let key st =
    let pairs = Array.init st.Gen.Length (fun i -> st.Gen.[i], st.Chip.[i])
    Array.sortInPlaceBy (fun (g,c) -> g, c) pairs
    let sb = System.Text.StringBuilder()
    sb.Append(st.Elevator) |> ignore
    for (g,c) in pairs do
        sb.Append('|').Append(g).Append(',').Append(c) |> ignore
    sb.ToString()

let valid st =
    let mutable ok = true
    for f = 0 to floors-1 do
        if ok then
            let mutable genSet = HashSet<int>()
            for i = 0 to st.Gen.Length-1 do
                if st.Gen.[i]=f then genSet.Add(i) |> ignore
            if genSet.Count>0 then
                for i = 0 to st.Chip.Length-1 do
                    if st.Chip.[i]=f && not (genSet.Contains i) then ok <- false
    ok

let done_ st = 
    Array.forall ((=)(floors-1)) st.Gen && Array.forall ((=)(floors-1)) st.Chip

let solve (lines:string[]) =
    let matId = Dictionary<string,int>()
    let gen = ResizeArray<int>()
    let chip = ResizeArray<int>()
    for f = 0 to floors-1 do
        let tokens = lines.[f].Split([|' ';',';'.'|], StringSplitOptions.RemoveEmptyEntries)
        for i = 0 to tokens.Length-1 do
            match tokens.[i] with
            | "generator" ->
                let name = tokens.[i-1]
                if not (matId.ContainsKey name) then matId.[name] <- matId.Count
                let id = matId.[name]
                while gen.Count <= id do gen.Add 0; chip.Add 0
                gen.[id] <- f
            | "microchip" ->
                let mutable name = tokens.[i-1]
                let dash = name.IndexOf '-'
                if dash>=0 then name <- name.Substring(0,dash)
                if not (matId.ContainsKey name) then matId.[name] <- matId.Count
                let id = matId.[name]
                while gen.Count <= id do gen.Add 0; chip.Add 0
                chip.[id] <- f
            | _ -> ()
    let start = { Elevator = 0; Gen = gen.ToArray(); Chip = chip.ToArray(); Steps = 0 }
    let q = Queue<State>()
    let seen = HashSet<string>()
    q.Enqueue start
    seen.Add (key start) |> ignore
    while q.Count>0 do
        let cur = q.Dequeue()
        if done_ cur then cur.Steps |> printfn "%d"; Environment.Exit 0
        let items = 
            [| for i = 0 to cur.Gen.Length-1 do if cur.Gen.[i]=cur.Elevator then yield (true,i)
               for i = 0 to cur.Chip.Length-1 do if cur.Chip.[i]=cur.Elevator then yield (false,i) |]
        let moves = ResizeArray<int[]>()
        for a = 0 to items.Length-1 do moves.Add [|a|]
        for a = 0 to items.Length-1 do
            for b = a+1 to items.Length-1 do moves.Add [|a;b|]
        for dir in [|1;-1|] do
            let nf = cur.Elevator + dir
            if nf>=0 && nf< floors then
                for mv in moves do
                    let ng = cur.Gen.Clone() :?> int[]
                    let nc = cur.Chip.Clone() :?> int[]
                    for idx in mv do
                        let isGen,i = items.[idx]
                        if isGen then ng.[i] <- nf else nc.[i] <- nf
                    let nxt = { Elevator = nf; Gen = ng; Chip = nc; Steps = cur.Steps+1 }
                    if valid nxt then
                        let k = key nxt
                        if seen.Add k then q.Enqueue nxt
    -1

[<EntryPoint>]
let main _ =
    let lines = File.ReadAllLines "input.txt"
    solve lines |> ignore
    0
