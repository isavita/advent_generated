open System
open System.IO
open System.Collections.Generic

type Instruction = { name: string; abcValues: int[] }

type OpcodeComputer = {
    mutable registers: int[]
    mutable boundRegister: int
    mutable instructions: Instruction[]
}

let opcodeNames = [| "addr"; "addi"; "mulr"; "muli"; "banr"; "bani"; "borr"; "bori"; "setr"; "seti"; "gtir"; "gtri"; "gtrr"; "eqir"; "eqri"; "eqrr" |]

let addr (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] + r.[a.[1]]
let addi (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] + a.[1]
let mulr (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] * r.[a.[1]]
let muli (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] * a.[1]
let banr (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] &&& r.[a.[1]]
let bani (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] &&& a.[1]
let borr (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] ||| r.[a.[1]]
let bori (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]] ||| a.[1]
let setr (r: int[]) (a: int[]) = r.[a.[2]] <- r.[a.[0]]
let seti (r: int[]) (a: int[]) = r.[a.[2]] <- a.[0]
let gtir (r: int[]) (a: int[]) = r.[a.[2]] <- if a.[0] > r.[a.[1]] then 1 else 0
let gtri (r: int[]) (a: int[]) = r.[a.[2]] <- if r.[a.[0]] > a.[1] then 1 else 0
let gtrr (r: int[]) (a: int[]) = r.[a.[2]] <- if r.[a.[0]] > r.[a.[1]] then 1 else 0
let eqir (r: int[]) (a: int[]) = r.[a.[2]] <- if a.[0] = r.[a.[1]] then 1 else 0
let eqri (r: int[]) (a: int[]) = r.[a.[2]] <- if r.[a.[0]] = a.[1] then 1 else 0
let eqrr (r: int[]) (a: int[]) = r.[a.[2]] <- if r.[a.[0]] = r.[a.[1]] then 1 else 0

let opcodeFuncs : (int[] -> int[] -> unit)[] = [| addr; addi; mulr; muli; banr; bani; borr; bori; setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr |]

let getOpcodeIndex (name: string) = Array.IndexOf(opcodeNames, name)

let parseInput (input: string) : OpcodeComputer =
    let s = input.Replace("\r","")
    let lines = s.Split('\n') |> Array.filter (fun l -> l.Length > 0)
    let ipLine = lines.[0]
    let ip =
        let p = ipLine.Split(' ')
        int p.[1]
    let instLines = if lines.Length > 1 then lines.[1..] else [||]
    let instructions =
        instLines
        |> Array.map (fun line ->
            let p = line.Split(' ')
            { name = p.[0]; abcValues = [| int p.[1]; int p.[2]; int p.[3] |] }
        )
    { registers = [|0;0;0;0;0;0|]; boundRegister = ip; instructions = instructions }

let tick (o: OpcodeComputer) : bool =
    if o.registers.[o.boundRegister] >= o.instructions.Length then true
    else
        let instIndex = o.registers.[o.boundRegister]
        let inst = o.instructions.[instIndex]
        let idx = getOpcodeIndex inst.name
        if idx = -1 then true
        else
            opcodeFuncs.[idx] o.registers inst.abcValues
            o.registers.[o.boundRegister] <- o.registers.[o.boundRegister] + 1
            o.registers.[o.boundRegister] >= o.instructions.Length

let solve (input: string) : int =
    let comp = parseInput input
    let seen = HashSet<int>()
    let mutable result = 0
    let mutable doneFlag = false
    while not doneFlag do
        if tick comp then
            doneFlag <- true
        else
            if comp.registers.[comp.boundRegister] = 28 then
                let reg5 = comp.registers.[5]
                if seen.Contains reg5 then
                    doneFlag <- true
                else
                    seen.Add reg5 |> ignore
                    result <- reg5
    result

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText("input.txt")
    let ans = solve input
    printfn "%d" ans
    0