
open System
open System.IO
open System.Text.RegularExpressions

type ValveInfo = 
    { id: string; 
      mutable flow: int; 
      mutable useful_idx: int }

let MAX_VALVES = 60
let MAX_TIME = 30
let MAX_USEFUL_VALVES = 16
let INF = 1000000

let valves = Array.init MAX_VALVES (fun _ -> { id = ""; flow = 0; useful_idx = -1 })
let dist = Array2D.init MAX_VALVES MAX_VALVES (fun i j -> if i = j then 0 else INF)
let num_valves = ref 0
let useful_indices = Array.zeroCreate MAX_USEFUL_VALVES
let num_useful = ref 0
let memo = Array3D.init MAX_VALVES (MAX_TIME + 1) (1 <<< MAX_USEFUL_VALVES) (fun _ _ _ -> -1L)

let get_valve_index id =
    let mutable idx = 0
    while idx < !num_valves && valves.[idx].id <> id do
        idx <- idx + 1
    if idx = !num_valves then
        valves.[idx] <- { id = id; flow = 0; useful_idx = -1 }
        num_valves := !num_valves + 1
    idx

let max_ll (a: int64) (b: int64) = if a > b then a else b

let rec solve (u_idx: int) (time_left: int) (mask: uint32) : int64 =
    if time_left <= 0 then 0L
    elif memo.[u_idx, time_left, int mask] <> -1L then memo.[u_idx, time_left, int mask]
    else
        let mutable max_pressure = 0L
        for i = 0 to !num_useful - 1 do
            if (mask &&& (uint32 (1 <<< i))) <> 0u then
                let v_idx = useful_indices.[i]
                let travel_time = dist.[u_idx, v_idx]
                let time_needed = travel_time + 1
                if time_left > time_needed then
                    let next_time_left = time_left - time_needed
                    let current_release = int64 valves.[v_idx].flow * int64 next_time_left
                    let next_mask = mask &&& ~~~(uint32 (1 <<< i))
                    max_pressure <- max_ll max_pressure (current_release + solve v_idx next_time_left next_mask)
        memo.[u_idx, time_left, int mask] <- max_pressure
        max_pressure

[<EntryPoint>]
let main argv =
    try
        use fp = File.OpenText "input.txt"
        let pattern = Regex @"Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.*)"
        while not fp.EndOfStream do
            let line = fp.ReadLine()
            let m = pattern.Match line
            if m.Success then
                let valve_id = m.Groups.[1].Value
                let flow = int m.Groups.[2].Value
                let u_idx = get_valve_index valve_id
                valves.[u_idx].flow <- flow
                let tunnels_str = m.Groups.[3].Value
                for token in tunnels_str.Split([|", "|], StringSplitOptions.None) do
                    let v_idx = get_valve_index token
                    dist.[u_idx, v_idx] <- 1
        // Floyd-Warshall algorithm
        for k = 0 to !num_valves - 1 do
            for i = 0 to !num_valves - 1 do
                for j = 0 to !num_valves - 1 do
                    if dist.[i, k] <> INF && dist.[k, j] <> INF then
                        let new_dist = dist.[i, k] + dist.[k, j]
                        if new_dist < dist.[i, j] then
                            dist.[i, j] <- new_dist
        // Useful indices
        for i = 0 to !num_valves - 1 do
            if valves.[i].flow > 0 then
                if !num_useful < MAX_USEFUL_VALVES then
                    valves.[i].useful_idx <- !num_useful
                    useful_indices.[!num_useful] <- i
                    num_useful := !num_useful + 1
                else
                    failwith "Exceeded MAX_USEFUL_VALVES limit."
        let start_idx = get_valve_index "AA"
        let initial_mask = (uint32 (1 <<< !num_useful)) - 1u
        let result = solve start_idx MAX_TIME initial_mask
        printfn "%d" result
        0
    with
    | ex -> 
        printfn "Error: %s" ex.Message
        1
