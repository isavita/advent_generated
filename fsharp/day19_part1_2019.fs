
open System
open System.IO

let readInput() =
    File.ReadAllText("input.txt").Split(',')
    |> Array.map int64
    |> Array.toList

type Vm =
    { Code : int64[]
      Ip : int
      Input : int64 list
      RelativeBase : int64 }

let getParamAddress vm pos mode =
    match mode with
    | 0 -> int vm.Code.[pos]
    | 1 -> pos
    | 2 -> int (vm.RelativeBase + vm.Code.[pos])
    | _ -> failwith "Invalid mode"

let rec run vm =
    let cmd = int vm.Code.[vm.Ip]
    let opCode = cmd % 100
    let modes = [ (cmd / 100) % 10; (cmd / 1000) % 10; (cmd / 10000) % 10 ]

    match opCode with
    | 1 ->
        let addr = getParamAddress vm (vm.Ip + 3) modes.[2]
        vm.Code.[addr] <- vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]] + vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]]
        run { vm with Ip = vm.Ip + 4 }
    | 2 ->
        let addr = getParamAddress vm (vm.Ip + 3) modes.[2]
        vm.Code.[addr] <- vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]] * vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]]
        run { vm with Ip = vm.Ip + 4 }
    | 3 ->
        let addr = getParamAddress vm (vm.Ip + 1) modes.[0]
        match vm.Input with
        | x :: rest ->
            vm.Code.[addr] <- x
            run { vm with Ip = vm.Ip + 2; Input = rest }
        | [] -> failwith "No input"
    | 4 ->
        let out = vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]]
        out, { vm with Ip = vm.Ip + 2 }
    | 5 ->
        let cond = vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]]
        let addr = int vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]]
        run { vm with Ip = if cond <> 0L then addr else vm.Ip + 3 }
    | 6 ->
        let cond = vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]]
        let addr = int vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]]
        run { vm with Ip = if cond = 0L then addr else vm.Ip + 3 }
    | 7 ->
        let addr = getParamAddress vm (vm.Ip + 3) modes.[2]
        vm.Code.[addr] <- if vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]] < vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]] then 1L else 0L
        run { vm with Ip = vm.Ip + 4 }
    | 8 ->
        let addr = getParamAddress vm (vm.Ip + 3) modes.[2]
        vm.Code.[addr] <- if vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]] = vm.Code.[getParamAddress vm (vm.Ip + 2) modes.[1]] then 1L else 0L
        run { vm with Ip = vm.Ip + 4 }
    | 9 ->
        let base' = vm.RelativeBase + vm.Code.[getParamAddress vm (vm.Ip + 1) modes.[0]]
        run { vm with Ip = vm.Ip + 2; RelativeBase = base' }
    | 99 -> -1L, vm
    | _ -> failwith "Invalid opCode"

let createVm code input =
    let code' = Array.zeroCreate 10000
    List.iteri (fun i v -> code'.[i] <- v) code
    { Code = code'; Ip = 0; Input = input; RelativeBase = 0L }

[<EntryPoint>]
let main _ =
    let code = readInput()
    let mutable sum = 0
    for y in 0..49 do
        for x in 0..49 do
            let vm = createVm code [int64 x; int64 y]
            let out, _ = run vm
            if out = 1L then sum <- sum + 1
    printfn "%d" sum
    0
