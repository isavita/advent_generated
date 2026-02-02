
open System
open System.IO

let MEM_SIZE = 2048
let INPUT_SIZE = 16
let OUTPUT_SIZE = 16

type Vm = 
    { code: int64 array
      mutable ip: int64
      mutable relativeBase: int64
      input: int64 list ref
      output: int64 list ref }

let vmInit() = 
    { code = Array.init MEM_SIZE (fun _ -> 0L)
      ip = 0L
      relativeBase = 0L
      input = ref []
      output = ref [] }

let vmLoad filename vm = 
    let lines = File.ReadAllLines(filename)
    let code = lines.[0].Split(',') |> Array.map int64
    Array.Copy(code, vm.code, code.Length)
    vm

let vmHasInput (vm: Vm) = not (List.isEmpty !vm.input)

let vmAddInput x (vm: Vm) = vm.input := !vm.input @ [x]

let vmHasOutput (vm: Vm) = not (List.isEmpty !vm.output)

let vmGetOutput (vm: Vm) = 
    match !vm.output with
    | x :: xs -> vm.output := xs; x
    | _ -> failwith "No output"

let vmGetParam (vm: Vm) pos mode = 
    match mode with
    | 0 -> 
        if vm.code.[pos] >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds at index %i" (int vm.code.[pos])
        vm.code.[int vm.code.[pos]]
    | 1 -> vm.code.[pos]
    | 2 -> 
        let addr = vm.code.[pos] + vm.relativeBase
        if addr >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds at index %i" (int addr)
        vm.code.[int addr]
    | _ -> failwithf "Error: invalid parameter mode: %i" mode

let vmGetParamAddr (vm: Vm) pos mode = 
    match mode with
    | 0 -> vm.code.[pos]
    | 1 -> failwithf "Error: writing to immediate mode at pos %i" (int pos)
    | 2 -> vm.code.[pos] + vm.relativeBase
    | _ -> failwithf "Error: invalid parameter mode: %i" mode

let rec vmRun (vm: Vm) = 
    let opcode = vm.code.[int vm.ip] % 100L
    let mode1 = int ((vm.code.[int vm.ip] / 100L) % 10L)
    let mode2 = int ((vm.code.[int vm.ip] / 1000L) % 10L)
    let mode3 = int ((vm.code.[int vm.ip] / 10000L) % 10L)

    match opcode with
    | 1L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        let addr = vmGetParamAddr vm (int (vm.ip + 3L)) mode3
        if addr < 0L || addr >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds, writing to address %i" (int addr)
        vm.code.[int addr] <- param1 + param2
        vm.ip <- vm.ip + 4L
        vmRun vm
    | 2L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        let addr = vmGetParamAddr vm (int (vm.ip + 3L)) mode3
        if addr < 0L || addr >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds, writing to address %i" (int addr)
        vm.code.[int addr] <- param1 * param2
        vm.ip <- vm.ip + 4L
        vmRun vm
    | 3L -> 
        if not (vmHasInput vm) then 
            ()
        else 
            let addr = vmGetParamAddr vm (int (vm.ip + 1L)) mode1
            if addr < 0L || addr >= int64 MEM_SIZE then 
                failwithf "Error: memory access out of bounds, writing to address %i" (int addr)
            vm.code.[int addr] <- List.head !vm.input
            vm.input := List.tail !vm.input
            vm.ip <- vm.ip + 2L
            vmRun vm
    | 4L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        vm.output := !vm.output @ [param1]
        vm.ip <- vm.ip + 2L
        vmRun vm
    | 5L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        if param1 <> 0L then 
            vm.ip <- param2
        else 
            vm.ip <- vm.ip + 3L
        vmRun vm
    | 6L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        if param1 = 0L then 
            vm.ip <- param2
        else 
            vm.ip <- vm.ip + 3L
        vmRun vm
    | 7L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        let addr = vmGetParamAddr vm (int (vm.ip + 3L)) mode3
        if addr < 0L || addr >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds, writing to address %i" (int addr)
        vm.code.[int addr] <- if param1 < param2 then 1L else 0L
        vm.ip <- vm.ip + 4L
        vmRun vm
    | 8L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        let param2 = vmGetParam vm (int (vm.ip + 2L)) mode2
        let addr = vmGetParamAddr vm (int (vm.ip + 3L)) mode3
        if addr < 0L || addr >= int64 MEM_SIZE then 
            failwithf "Error: memory access out of bounds, writing to address %i" (int addr)
        vm.code.[int addr] <- if param1 = param2 then 1L else 0L
        vm.ip <- vm.ip + 4L
        vmRun vm
    | 9L -> 
        let param1 = vmGetParam vm (int (vm.ip + 1L)) mode1
        vm.relativeBase <- vm.relativeBase + param1
        vm.ip <- vm.ip + 2L
        vmRun vm
    | 99L -> ()
    | _ -> failwithf "Error: unknown opcode: %i at position %i" (int opcode) (int vm.ip)

let beam x y = 
    let vm = vmInit() |> vmLoad "input.txt"
    vmAddInput (int64 x) vm
    vmAddInput (int64 y) vm
    vmRun vm
    vmHasOutput vm && vmGetOutput vm = 1L

let main() = 
    let mutable y = 20
    let mutable x = 0
    while true do
        if not (beam x y) then 
            x <- x + 1
        elif not (beam (x + 99) y) then 
            y <- y + 1
        elif not (beam x (y + 99)) then 
            x <- x + 1
        else 
            printfn "%i" (x * 10000 + y)
            exit 0

main()
