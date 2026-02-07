// Program.fs
// AoC 2019 Day 25 (Intcode text adventure) automated solver (F#)
//
// Build/run:
//   dotnet new console -lang F# -n Day25 -o Day25
//   replace Day25/Program.fs with this file
//   dotnet run --project Day25 -- input.txt
//
// Or run directly:
//   dotnet fsi Program.fs -- input.txt

module Program

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions

// ----------------------------- Intcode Emulator -----------------------------

type EmuStatus =
    | Halted
    | Output of int64
    | WaitingForInput

type Emulator =
    { Mem: Dictionary<int64, int64>
      Input: Queue<int64>
      mutable Ip: int64
      mutable Rb: int64 }

let emuNew (program: int64 list) : Emulator =
    let mem = Dictionary<int64, int64>(program.Length * 2 + 16)
    program |> List.iteri (fun i v -> mem.[int64 i] <- v)
    { Mem = mem; Input = Queue<int64>(); Ip = 0L; Rb = 0L }

let memGet (e: Emulator) (addr: int64) =
    match e.Mem.TryGetValue(addr) with
    | true, v -> v
    | _ -> 0L

let memSet (e: Emulator) (addr: int64) (v: int64) =
    e.Mem.[addr] <- v

let writeString (e: Emulator) (s: string) =
    for ch in s do e.Input.Enqueue(int64 (int ch))

let pow10 (n: int) =
    let mutable p = 1L
    for _ = 1 to n do p <- p * 10L
    p

let getMode (instruction: int64) (offset: int) =
    // offset is 1-based parameter index
    (instruction / pow10 (offset + 1)) % 10L

let getParam (e: Emulator) (instruction: int64) (offset: int) =
    let mode = getMode instruction offset
    let param = memGet e (e.Ip + int64 offset)
    match mode with
    | 0L -> memGet e param
    | 1L -> param
    | 2L -> memGet e (e.Rb + param)
    | _ -> failwithf "Unknown parameter mode %d at ip %d" mode e.Ip

let getWriteAddr (e: Emulator) (instruction: int64) (offset: int) =
    let mode = getMode instruction offset
    let param = memGet e (e.Ip + int64 offset)
    match mode with
    | 0L -> param
    | 2L -> e.Rb + param
    | _ -> failwithf "Invalid mode for writing %d at ip %d" mode e.Ip

let emulate (e: Emulator) : EmuStatus =
    let rec loop () =
        let instruction = memGet e e.Ip
        let opcode = instruction % 100L

        match opcode with
        | 1L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            let c = getWriteAddr e instruction 3
            memSet e c (a + b)
            e.Ip <- e.Ip + 4L
            loop ()
        | 2L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            let c = getWriteAddr e instruction 3
            memSet e c (a * b)
            e.Ip <- e.Ip + 4L
            loop ()
        | 3L ->
            if e.Input.Count = 0 then
                WaitingForInput
            else
                let addr = getWriteAddr e instruction 1
                memSet e addr (e.Input.Dequeue())
                e.Ip <- e.Ip + 2L
                loop ()
        | 4L ->
            let a = getParam e instruction 1
            e.Ip <- e.Ip + 2L
            Output a
        | 5L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            e.Ip <- (if a <> 0L then b else e.Ip + 3L)
            loop ()
        | 6L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            e.Ip <- (if a = 0L then b else e.Ip + 3L)
            loop ()
        | 7L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            let c = getWriteAddr e instruction 3
            memSet e c (if a < b then 1L else 0L)
            e.Ip <- e.Ip + 4L
            loop ()
        | 8L ->
            let a = getParam e instruction 1
            let b = getParam e instruction 2
            let c = getWriteAddr e instruction 3
            memSet e c (if a = b then 1L else 0L)
            e.Ip <- e.Ip + 4L
            loop ()
        | 9L ->
            let a = getParam e instruction 1
            e.Rb <- e.Rb + a
            e.Ip <- e.Ip + 2L
            loop ()
        | 99L -> Halted
        | _ -> failwithf "Unknown opcode %d at ip %d" opcode e.Ip

    loop ()

// ----------------------------- World / Solver -----------------------------

type Room =
    { Name: string
      Connections: Dictionary<string, string option> } // dir -> Some roomName | None (unexplored)

type Mode =
    | Explore
    | Navigate
    | Test

type State =
    { Emulator: Emulator
      World: Dictionary<string, Room>
      Inventory: Dictionary<string, bool>
      mutable Mode: Mode
      mutable CurrentRoom: string option
      mutable Checkpoint: string option
      mutable Floor: string option
      mutable TestDir: string
      // used as stack in Explore (push/pop at end), and as queue in Navigate (pop at front)
      Path: List<string>
      mutable AvailableItems: string list
      mutable ItemMask: int
      mutable LastRoom: string option
      mutable LastItems: string list
      mutable LastDir: string
      Output: StringBuilder }

let opposite =
    dict [ ("north","south"); ("south","north"); ("west","east"); ("east","west") ]

let blacklist =
    set [ "photons"; "escape pod"; "molten lava"; "infinite loop"; "giant electromagnet" ]

let getRoom (st: State) (name: string) : Room =
    match st.World.TryGetValue(name) with
    | true, r -> r
    | _ ->
        let r = { Name = name; Connections = Dictionary<string, string option>() }
        st.World.[name] <- r
        r

let ensureRoom (st: State) (name: string) =
    ignore (getRoom st name)

let setDoor (st: State) (dir: string) =
    match st.CurrentRoom with
    | None -> ()
    | Some rn ->
        let r = getRoom st rn
        if not (r.Connections.ContainsKey(dir)) then
            r.Connections.[dir] <- None

let updateConnections (st: State) =
    match st.LastRoom, st.CurrentRoom with
    | Some lastN, Some curN when st.LastDir <> "" ->
        let lastR = getRoom st lastN
        let curR = getRoom st curN
        let mutable existing = Unchecked.defaultof<string option>
        let has = lastR.Connections.TryGetValue(st.LastDir, &existing)
        let shouldLink =
            if not has then true
            else match existing with | None -> true | Some _ -> false
        if shouldLink then
            lastR.Connections.[st.LastDir] <- Some curN
            curR.Connections.[opposite.[st.LastDir]] <- Some lastN
    | _ -> ()

let handleAlert (st: State) =
    if st.Mode = Explore then
        // pop stack (end) like Kotlin dropLast(1)
        if st.Path.Count > 0 then st.Path.RemoveAt(st.Path.Count - 1)

        st.Checkpoint <- st.LastRoom
        st.Floor <- st.CurrentRoom
        st.TestDir <- st.LastDir

        match st.Checkpoint, st.Floor with
        | Some cp, Some fl when st.TestDir <> "" ->
            let checkpointRoom = getRoom st cp
            checkpointRoom.Connections.[st.TestDir] <- Some fl
        | _ -> ()

    st.LastRoom <- None
    st.LastItems <- []
    st.LastDir <- ""

let tryDirectionTo (st: State) (fromName: string) (toName: string) : string option =
    let r = getRoom st fromName
    let mutable found: string option = None
    for kv in r.Connections do
        match kv.Value with
        | Some dest when dest = toName && found.IsNone ->
            found <- Some kv.Key
        | _ -> ()
    found

let chooseUnexplored (st: State) (roomName: string) : string option =
    let r = getRoom st roomName
    let preferred = [| "north"; "south"; "west"; "east" |]
    let mutable chosen: string option = None
    for d in preferred do
        if chosen.IsNone then
            let mutable v = Unchecked.defaultof<string option>
            if r.Connections.TryGetValue(d, &v) then
                if v.IsNone then chosen <- Some d
    if chosen.IsSome then chosen
    else
        let mutable any: string option = None
        for kv in r.Connections do
            if any.IsNone && kv.Value.IsNone then any <- Some kv.Key
        any

let findPath (st: State) (fromName: string) (toName: string) : string list option =
    let q = Queue<string * string list>()
    q.Enqueue(fromName, [fromName])
    let visited = HashSet<string>()
    visited.Add(fromName) |> ignore

    let mutable result: string list option = None
    while q.Count > 0 && result.IsNone do
        let (node, path) = q.Dequeue()
        if node = toName then
            result <- Some path
        else
            let r = getRoom st node
            for kv in r.Connections do
                match kv.Value with
                | Some n when visited.Add(n) ->
                    q.Enqueue(n, path @ [n])
                | _ -> ()
    result

let sendCommand (st: State) (cmd: string) =
    writeString st.Emulator cmd

// ----------------------------- Parsing Output -----------------------------

let processOutput (st: State) (output: string) : string list =
    let lines = output.Split([| '\n' |], StringSplitOptions.None)
    let mutable items: string list = []
    let mutable i = 0

    while i < lines.Length do
        let line = lines.[i].Trim()
        if line = "" || line = "Command?" then
            i <- i + 1

        elif line.StartsWith("== ") && line.EndsWith(" ==") && line.Length >= 6 then
            let name = line.Substring(3, line.Length - 6)
            ensureRoom st name
            st.CurrentRoom <- Some name
            items <- []
            i <- i + 1
            while i < lines.Length && lines.[i].Trim() <> "" do
                i <- i + 1

        elif line = "Doors here lead:" then
            i <- i + 1
            while i < lines.Length && lines.[i].Trim() <> "" do
                let dl = lines.[i].Trim()
                if dl.StartsWith("- ") then
                    setDoor st (dl.Substring(2))
                i <- i + 1

        elif line = "Items here:" then
            i <- i + 1
            let acc = ResizeArray<string>()
            while i < lines.Length && lines.[i].Trim() <> "" do
                let il = lines.[i].Trim()
                if il.StartsWith("- ") then
                    acc.Add(il.Substring(2))
                i <- i + 1
            items <- List.ofSeq acc

        elif line.StartsWith("You take the ") && line.EndsWith(".") then
            let taken =
                line.Substring("You take the ".Length,
                               line.Length - "You take the ".Length - 1)
            st.Inventory.[taken] <- true
            match st.LastRoom with
            | Some lr ->
                st.CurrentRoom <- Some lr
                items <- st.LastItems |> List.filter (fun x -> x <> taken)
            | None -> ()
            i <- i + 1

        elif line.StartsWith("You drop the ") && line.EndsWith(".") then
            let dropped =
                line.Substring("You drop the ".Length,
                               line.Length - "You drop the ".Length - 1)
            st.Inventory.[dropped] <- false
            match st.LastRoom with
            | Some lr ->
                st.CurrentRoom <- Some lr
                items <- st.LastItems @ [dropped]
            | None -> ()
            i <- i + 1

        elif line.StartsWith("A loud, robotic voice says \"Alert!") then
            handleAlert st
            i <- i + 1

        else
            i <- i + 1

    items

let extractCode (output: string) : string option =
    let m = Regex.Match(output, "Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\.", RegexOptions.Singleline)
    if m.Success then Some m.Groups.[1].Value else None

// ----------------------------- Action Selection -----------------------------

let explore (st: State) (items: string list) =
    // take first safe item
    match items |> List.tryFind (fun it -> not (blacklist.Contains it)) with
    | Some it ->
        sendCommand st (sprintf "take %s\n" it)
    | None ->
        match st.CurrentRoom with
        | None ->
            // bootstrap in case output parsing didn't see a room yet
            st.LastDir <- "north"
            sendCommand st "north\n"
        | Some cur ->
            match chooseUnexplored st cur with
            | Some dir ->
                st.Path.Add(cur)              // push
                st.LastDir <- dir
                sendCommand st (dir + "\n")
            | None ->
                if st.Path.Count > 0 then
                    let prev = st.Path.[st.Path.Count - 1]
                    st.Path.RemoveAt(st.Path.Count - 1) // pop
                    match tryDirectionTo st cur prev with
                    | Some back ->
                        st.LastDir <- back
                        sendCommand st (back + "\n")
                    | None ->
                        failwithf "Cannot go from \"%s\" to \"%s\"" cur prev
                else
                    match st.Checkpoint, st.Floor with
                    | Some cp, Some _ ->
                        match findPath st cur cp with
                        | None -> failwith "No path to checkpoint"
                        | Some p ->
                            st.Path.Clear()
                            // drop current (head) => enqueue remaining in order
                            for idx = 1 to p.Length - 1 do st.Path.Add(p.[idx])
                            st.Mode <- Navigate
                    | _ ->
                        failwith "No checkpoint found"

let navigate (st: State) =
    match st.CurrentRoom with
    | None ->
        st.LastDir <- "north"
        sendCommand st "north\n"
    | Some cur ->
        if st.Path.Count = 0 then
            let avail =
                st.Inventory
                |> Seq.filter (fun kv -> kv.Value)
                |> Seq.map (fun kv -> kv.Key)
                |> Seq.sort
                |> Seq.toList
            st.AvailableItems <- avail
            st.ItemMask <- 0
            st.Mode <- Test
        else
            let next = st.Path.[0]
            st.Path.RemoveAt(0) // pop front
            match tryDirectionTo st cur next with
            | Some dir ->
                st.LastDir <- dir
                sendCommand st (dir + "\n")
            | None ->
                failwithf "Cannot go from \"%s\" to \"%s\"" cur next

let testItems (st: State) =
    let items = st.AvailableItems
    let mask = st.ItemMask
    let maxMask = 1 <<< items.Length
    if mask >= maxMask then
        failwithf "No valid item combination found after %d attempts" maxMask

    let mutable changed = false

    for idx = 0 to items.Length - 1 do
        if not changed then
            let item = items.[idx]
            let target = (mask &&& (1 <<< idx)) <> 0
            let mutable cur = false
            let has = st.Inventory.TryGetValue(item, &cur)
            let curState = if has then cur else false

            if curState <> target then
                let action = if target then "take" else "drop"
                sendCommand st (sprintf "%s %s\n" action item)
                changed <- true

    if not changed then
        st.ItemMask <- mask + 1
        if st.TestDir = "" then failwith "Test direction not set"
        sendCommand st (st.TestDir + "\n")

// ----------------------------- Main Loop -----------------------------

[<EntryPoint>]
let main argv =
    let filename = if argv.Length > 0 then argv.[0] else "input.txt"
    let program =
        File.ReadAllText(filename).Trim().Split(',')
        |> Array.map (fun s -> Int64.Parse(s.Trim()))
        |> Array.toList

    let emu = emuNew program

    let st =
        { Emulator = emu
          World = Dictionary<string, Room>()
          Inventory = Dictionary<string, bool>()
          Mode = Explore
          CurrentRoom = None
          Checkpoint = None
          Floor = None
          TestDir = ""
          Path = List<string>()
          AvailableItems = []
          ItemMask = 0
          LastRoom = None
          LastItems = []
          LastDir = ""
          Output = StringBuilder(4096) }

    let maxActions = 30000
    let mutable actions = 0
    let mutable done_ = false

    while not done_ do
        match emulate st.Emulator with
        | Halted ->
            let out = st.Output.ToString()
            match extractCode out with
            | Some code ->
                Console.WriteLine(code)
                done_ <- true
            | None ->
                Console.Error.WriteLine("Error: No solution found")
                Console.Error.WriteLine(out)
                done_ <- true

        | Output ch ->
            st.Output.Append(char (int ch)) |> ignore

        | WaitingForInput ->
            actions <- actions + 1
            if actions > maxActions then failwith "Loop detected (action cap reached)"

            let out = st.Output.ToString()
            st.Output.Clear() |> ignore

            let items = processOutput st out
            updateConnections st

            st.LastRoom <- st.CurrentRoom
            st.LastItems <- items
            st.LastDir <- ""

            match st.Mode with
            | Explore -> explore st items
            | Navigate -> navigate st
            | Test -> testItems st

    0
