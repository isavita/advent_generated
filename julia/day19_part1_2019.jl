
using Printf

mutable struct VM
    code::Dict{Int, Int}
    ip::Int
    relative_base::Int
    input::Vector{Int}
    output::Vector{Int}
end

function load_code(filename::String)::Dict{Int, Int}
    code = Dict{Int, Int}()
    s = open(filename) do f
        read(f, String)
    end
    parts = split(strip(s), ',')
    for (i, item) in enumerate(parts)
        code[i-1] = parse(Int, item) # 0-based indexing like Python example
    end
    return code
end

function get_param(vm::VM, index::Int, modes::Vector{Int})::Int
    mode = modes[index]
    val = get(vm.code, vm.ip + index, 0)
    if mode == 0       # Position mode
        return get(vm.code, val, 0)
    elseif mode == 1   # Immediate mode
        return val
    elseif mode == 2   # Relative mode
        return get(vm.code, vm.relative_base + val, 0)
    else
        error("Unknown parameter mode: $mode")
    end
end

function get_address(vm::VM, index::Int, modes::Vector{Int})::Int
    mode = modes[index]
    val = get(vm.code, vm.ip + index, 0)
    if mode == 0       # Position mode
        return val
    elseif mode == 2   # Relative mode
        return vm.relative_base + val
    else
        error("Unknown address mode: $mode")
    end
end

function run!(vm::VM)
    while true
        cmd = get(vm.code, vm.ip, 0)
        opcode = cmd % 100
        modes = [(cmd ÷ 10^(i+1)) % 10 for i in 1:3] # modes for param 1, 2, 3

        if opcode == 1      # add
            addr = get_address(vm, 3, modes)
            vm.code[addr] = get_param(vm, 1, modes) + get_param(vm, 2, modes)
            vm.ip += 4
        elseif opcode == 2  # multiply
            addr = get_address(vm, 3, modes)
            vm.code[addr] = get_param(vm, 1, modes) * get_param(vm, 2, modes)
            vm.ip += 4
        elseif opcode == 3  # input
            if isempty(vm.input)
                error("Input requested but none available")
            end
            addr = get_address(vm, 1, modes)
            vm.code[addr] = popfirst!(vm.input)
            vm.ip += 2
        elseif opcode == 4  # output
            push!(vm.output, get_param(vm, 1, modes))
            vm.ip += 2
        elseif opcode == 5  # jump-if-true
            if get_param(vm, 1, modes) != 0
                vm.ip = get_param(vm, 2, modes)
            else
                vm.ip += 3
            end
        elseif opcode == 6  # jump-if-false
            if get_param(vm, 1, modes) == 0
                vm.ip = get_param(vm, 2, modes)
            else
                vm.ip += 3
            end
        elseif opcode == 7  # less than
            addr = get_address(vm, 3, modes)
            vm.code[addr] = Int(get_param(vm, 1, modes) < get_param(vm, 2, modes))
            vm.ip += 4
        elseif opcode == 8  # equals
            addr = get_address(vm, 3, modes)
            vm.code[addr] = Int(get_param(vm, 1, modes) == get_param(vm, 2, modes))
            vm.ip += 4
        elseif opcode == 9  # adjust relative base
            vm.relative_base += get_param(vm, 1, modes)
            vm.ip += 2
        elseif opcode == 99 # halt
            break
        else
            error("Unknown opcode $opcode at ip $(vm.ip)")
        end
    end
end

function beam(x::Int, y::Int, initial_code::Dict{Int, Int})::Bool
    # Create a fresh VM instance for each call, copying the initial code
    vm = VM(copy(initial_code), 0, 0, [x, y], Int[])
    run!(vm)
    return !isempty(vm.output) && vm.output[1] == 1
end

function main()
    initial_code = load_code("input.txt")
    count = 0
    for y in 0:49
        for x in 0:49
            if beam(x, y, initial_code)
                count += 1
            end
        end
    end
    println(count)
end

main()
